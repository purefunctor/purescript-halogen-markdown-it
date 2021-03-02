module Halogen.Markdown.Transformer where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.List (List(..))
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, new)
import Halogen.HTML as HH
import Halogen.Markdown.AST (Markdown)
import Unsafe.Coerce (unsafeCoerce)


data TransformerF n
  = ToHalogen Markdown n
  | NextLine (Maybe Markdown → n)
  | PushStack Markdown n
  | PopStack (Maybe Markdown → n)

derive instance functorTransformerF ∷ Functor TransformerF


type TransformerM = Free TransformerF


toHalogen ∷ Markdown → TransformerM Unit
toHalogen = liftF <<< flip ToHalogen unit


nextLine ∷ TransformerM (Maybe Markdown)
nextLine = liftF $ NextLine identity


pushElem ∷ Markdown → TransformerM Unit
pushElem = liftF <<< flip PushStack unit


popElem ∷ TransformerM (Maybe Markdown)
popElem = liftF $ PopStack identity


data Element = Element (∀ w a. HH.HTML w a)


type MachineEnv =
  { lines ∷ Ref (List Markdown)
  , stack ∷ Ref (List Markdown)
  , elems ∷ Ref (List Element)
  }


type MachineT = ReaderT MachineEnv


runMachineT ∷
  ∀ m r
  . MonadEffect m
  ⇒ MachineT m r
  → MachineEnv
  → m r
runMachineT = runReaderT


runTransformerM ∷
  ∀ m r
  . MonadEffect m
  ⇒ MonadRec m
  ⇒ TransformerM r
  → List Markdown
  → m r
runTransformerM dsl mds = do
  env <- liftEffect do
    lines <- new mds
    stack <- new Nil
    elems <- new Nil
    pure { lines, stack, elems }
  runMachineT (runFreeM go dsl) env
  where
    go ∷ TransformerF (TransformerM r) → MachineT m (TransformerM r)
    go = unsafeCoerce
