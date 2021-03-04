module Halogen.Markdown.AST where

import Prelude

import DOM.HTML.Indexed (Interactive)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)


data Markdown
  = Text String
  | Heading Level String
  | CodeBlock String String
  | BlankLine


derive instance eqMarkdown ∷ Eq Markdown


instance showMd :: Show Markdown where
  show (Text t) = "Text: " <> t
  show (Heading h t) = "Heading: " <> show h <> " " <> t
  show (CodeBlock l t) = "CodeBlock: " <> l <> " " <> t
  show BlankLine = "BlankLine"


data Level = H1 | H2 | H3 | H4 | H5 | H6


derive instance eqLevel ∷ Eq Level
derive instance ordLevel ∷ Ord Level


instance showLevel :: Show Level where
  show H1 = "H1"
  show H2 = "H2"
  show H3 = "H3"
  show H4 = "H4"
  show H5 = "H5"
  show H6 = "H6"


toLevel ∷ Int → Maybe Level
toLevel 1 = Just H1
toLevel 2 = Just H2
toLevel 3 = Just H3
toLevel 4 = Just H4
toLevel 5 = Just H5
toLevel 6 = Just H6
toLevel _ = Nothing


toHeader ∷
  ∀ w i
  . Level
  → Array ( HP.IProp ( Interactive ( onScroll :: Event ) ) i )
  → Array ( HH.HTML w i )
  → HH.HTML w i
toHeader H1 = HH.h1
toHeader H2 = HH.h2
toHeader H3 = HH.h3
toHeader H4 = HH.h4
toHeader H5 = HH.h5
toHeader H6 = HH.h6
