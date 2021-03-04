module Halogen.Markdown.Transformer where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader.Trans (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.ST (ST, run)
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Ref (STRef, modify, new, read)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.Markdown.AST (Markdown(..), toHeader)
import Halogen.Markdown.Parser (parseMarkdown)


data TransformerF n
  = ToHalogen Markdown n
  | NextLine (Maybe Markdown → n)
  | PushStack Markdown n
  | PopStack (Maybe Markdown → n)
  | PeekStack (Maybe Markdown → n)
  | FlushResult (List Element → n)

derive instance functorTransformerF ∷ Functor TransformerF


type TransformerM = Free TransformerF


toHalogen ∷ Markdown → TransformerM Unit
toHalogen = liftF <<< flip ToHalogen unit


nextLine ∷ TransformerM (Maybe Markdown)
nextLine = liftF $ NextLine identity


pushStack ∷ Markdown → TransformerM Unit
pushStack = liftF <<< flip PushStack unit


popStack ∷ TransformerM (Maybe Markdown)
popStack = liftF $ PopStack identity


peekStack ∷ TransformerM (Maybe Markdown)
peekStack = liftF $ PeekStack identity


flushResult ∷ TransformerM (List Element)
flushResult = liftF $ FlushResult identity


newtype Element = Element (∀ w a. HH.HTML w a)


unElement ∷ Element → (∀ w a. HH.HTML w a)
unElement (Element element) = element


type MachineEnv s =
  { lines ∷ STRef s (List Markdown)
  , stack ∷ STRef s (List Markdown)
  , elems ∷ STRef s (List Element)
  }


type MachineT s = ReaderT (MachineEnv s)


runMachineT ∷
  ∀ m s r
  . MonadST s m
  ⇒ MachineT s m r
  → MachineEnv s
  → m r
runMachineT = runReaderT


runTransformerM ∷
  ∀ m s r
  . MonadST s m
  ⇒ TransformerM r
  → List Markdown
  → m r
runTransformerM dsl mds = liftST $ do
  lines <- new mds
  stack <- new Nil
  elems <- new Nil
  runMachineT (runFreeM go dsl) { lines, stack, elems }
  where
    go :: TransformerF (TransformerM r) -> MachineT s (ST s) (TransformerM r)
    go instruction = do
      env <- ask
      lift $ case instruction of
        ToHalogen e n -> do
          elems <- read env.elems

          case e of
            Text text ->
              pushHalogen ( HH.p [ ] [ HH.text text ] )
            Heading level text ->
              pushHalogen ( (toHeader level) [ ] [ HH.text text ] )
            BlankLine ->
              pure unit
            CodeBlock language text ->
              pushHalogen ( HH.pre [ ] [ HH.code [ ] [ HH.text text ] ] )

          pure n
          where
            pushHalogen ∷ (∀ w a. HH.HTML w a) → ST s Unit
            pushHalogen element =
              void $ modify ( Cons ( Element element ) ) env.elems

        NextLine f -> do
          lines <- read env.lines

          case List.head lines of
            Just e -> do
              void $ modify (List.drop 1) env.lines
              pure (f $ Just e)
            Nothing ->
              pure $ f Nothing

        PushStack e n ->
          modify (\es -> e : es) env.stack *> pure n

        PopStack f -> do
          stack <- read env.stack

          case List.head stack of
            Just e -> do
              void $ modify (List.drop 1) env.stack
              pure (f $ Just e)
            Nothing ->
              pure $ f Nothing

        PeekStack f -> do
          stack <- read env.stack
          pure $ f $ List.head stack

        FlushResult f -> do
          elems <- read env.elems
          pure $ f elems


mkHTML ∷ ∀ w a. String → Array (HH.HTML w a)
mkHTML markdown = run do

  case parseMarkdown markdown of

    Right ast -> do
      elements <- runTransformerM (normalize *> transform) ast
      pure $ Array.fromFoldable $ unElement <$> elements

    _ -> pure []

  where
    normalize = untilJust do
      mLine <- nextLine

      case mLine of

        Just line@(Text text) -> do
          mTop <- peekStack

          line' <- case mTop of
            Just (Text text') -> do
              _ <- popStack
              pure $ Text $ text <> "\n" <> text
            _ ->
              pure line

          pushStack line'
          pure Nothing

        Just line -> do
          pushStack line
          pure Nothing

        Nothing -> do
          pure $ Just unit

    transform = untilJust do
      mLine <- popStack

      case mLine of

        Just line -> do
          toHalogen line
          pure Nothing

        Nothing -> do
          result <- flushResult
          pure $ Just result
