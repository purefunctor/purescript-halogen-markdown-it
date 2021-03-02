module Halogen.Markdown where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import DOM.HTML.Indexed (Interactive)
import Data.Either (Either)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, new, read)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (char, eof, regex, skipSpaces, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (choice, many)
import Web.Event.Event (Event)


data MarkdownAST
  = Heading Level String
  | Text String
  | BlankLine
  | Block (List MarkdownAST)

derive instance eqMarkdownAST :: Eq MarkdownAST

instance showMarkdownAST :: Show MarkdownAST where
  show = case _ of
    Heading level text -> List.intercalate " " [ "Heading", show level, text ]
    Text text -> List.intercalate " " [ "Text", text ]
    BlankLine -> "BlankLine"
    Block text -> List.intercalate " " [ "Block", show text ]


data Level = H1 | H2 | H3 | H4 | H5 | H6

derive instance eqLevel :: Eq Level
derive instance ordLevel :: Ord Level

instance showLevel :: Show Level where
  show H1 = "Level 1"
  show H2 = "Level 2"
  show H3 = "Level 3"
  show H4 = "Level 4"
  show H5 = "Level 5"
  show H6 = "Level 6"

toLevel :: Int -> Maybe Level
toLevel 1 = Just H1
toLevel 2 = Just H2
toLevel 3 = Just H3
toLevel 4 = Just H4
toLevel 5 = Just H5
toLevel 6 = Just H6
toLevel _ = Nothing

type HTMLheader = Interactive ( onScroll :: Event )

toHeader
  :: forall w i
   . Level
  -> Array (HP.IProp HTMLheader i)
  -> Array (HH.HTML w i)
  -> HH.HTML w i
toHeader H1 = HH.h1
toHeader H2 = HH.h2
toHeader H3 = HH.h3
toHeader H4 = HH.h4
toHeader H5 = HH.h5
toHeader H6 = HH.h6


pHeader :: Parser MarkdownAST
pHeader = do
  leading <- String.length <$> whiteSpace

  when (leading > 3) (fail "Invalid heading")

  level <- String.length <$> regex "#+"
  skipSpaces
  title <- regex ".*" <* (many $ char '\n')

  case toLevel level of
    Just level' ->
      pure $ Heading level' title
    Nothing ->
      fail "Invalid heading"


pBlank :: Parser MarkdownAST
pBlank = char '\n' *> pure BlankLine


pText :: Parser MarkdownAST
pText = Text <$> (skipSpaces *> text <* ((void $ char '\n') <|> eof))
  where
    text = choice $ try <$>
      [ (<>) <$> pure "#" <*> (string "\\#" *> regex ".+")
      , regex ".+"
      ]


pBlock :: Parser MarkdownAST
pBlock = Block <$> (many $ choice $ try <$> [pHeader, pBlank, pText])


parseMarkdown :: String -> Either ParseError MarkdownAST
parseMarkdown = runParser pBlock


data MachineF n
  = PlaceElem MarkdownAST n
  | NextLine (Maybe MarkdownAST -> n)
  | PushElem MarkdownAST n
  | PopElem (Maybe MarkdownAST -> n)

derive instance functorMachineF :: Functor MachineF


type MachineM = Free MachineF


placeElem :: MarkdownAST -> MachineM Unit
placeElem = liftF <<< flip PlaceElem unit


nextLine :: MachineM (Maybe MarkdownAST)
nextLine = liftF $ NextLine identity


pushElem :: MarkdownAST -> MachineM Unit
pushElem = liftF <<< flip PushElem unit


popElem :: MachineM (Maybe MarkdownAST)
popElem = liftF $ PopElem identity


data Element = Element (forall w a. HH.HTML w a)


type TransformerE =
  { lines :: Ref (List MarkdownAST)
  , stack :: Ref (List MarkdownAST)
  , elems :: Ref (List Element)
  }


type TransformerT m r = ReaderT TransformerE m r


runTransformerM
  :: forall m r
   . MonadEffect m
  => TransformerT m r
  -> TransformerE
  -> m r
runTransformerM = runReaderT


runMachine
  :: forall m r
   . MonadEffect m
  => MonadRec m
  => MachineM r
  -> List MarkdownAST
  -> m r
runMachine m l = do
  lines <- liftEffect $ new l
  stack <- liftEffect $ new Nil
  elems <- liftEffect $ new Nil
  runTransformerM (runFreeM go m) { lines, stack, elems }
  where
    go :: MachineF (MachineM r) -> TransformerT m (MachineM r)
    go instruction = do
      state <- ask

      case instruction of
        PlaceElem e n -> liftEffect do
          elems <- read state.elems

          case e of
            Heading level text -> do
              modify_ (\es -> Element ((toHeader level) [ ] [ HH.text text ]) : es) state.elems
            Text text -> do
              modify_ (\es -> Element (HH.p [ ] [ HH.text text ]) : es) state.elems
            _ -> pure unit

          pure n

        NextLine f -> liftEffect do
          lines <- read state.lines

          case List.head lines of
            Just e -> do
              modify_ (List.drop 1) state.lines *> pure (f $ Just e)
            Nothing ->
              pure $ f Nothing

        PushElem e n -> liftEffect $
          modify_ (\es -> e : es) state.stack *> pure n

        PopElem f -> liftEffect do
          stack <- read state.stack

          case List.head stack of
            Just e -> do
              modify_ (List.drop 1) state.stack *> pure (f $ Just e)
            Nothing ->
              pure $ f Nothing
