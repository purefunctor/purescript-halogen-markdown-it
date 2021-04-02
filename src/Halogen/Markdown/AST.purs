module Halogen.Markdown.AST where

import Prelude

import DOM.HTML.Indexed (Interactive)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Halogen.HTML as HH
import Web.Event.Event (Event)

{-----------------------------------------------------------------------}

data Line
  = TextLine Text
  | BlankLine
  | Heading Level Text
  | CodeBlock Language Text

derive instance eqLine :: Eq Line
derive instance genericLine :: Generic Line _

instance showLine :: Show Line where
  show = genericShow

type Lines = List Line

{-----------------------------------------------------------------------}

newtype Text = Text String

derive newtype instance eqText :: Eq Text
derive newtype instance showText :: Show Text
derive newtype instance semigroupText :: Semigroup Text
derive newtype instance monoidText :: Monoid Text
derive instance newtypeText :: Newtype Text _

{-----------------------------------------------------------------------}

data Level = H1 | H2 | H3 | H4 | H5 | H6

derive instance eqLevel :: Eq Level
derive instance ordLevel :: Ord Level
derive instance genericLevel :: Generic Level _

instance showLevel :: Show Level where
  show = genericShow

toLevel :: Int -> Maybe Level
toLevel 1 = Just H1
toLevel 2 = Just H2
toLevel 3 = Just H3
toLevel 4 = Just H4
toLevel 5 = Just H5
toLevel 6 = Just H6
toLevel _ = Nothing

toHeader ::
  forall w a
  . Level
  -> Array ( HH.IProp ( Interactive ( onScroll :: Event ) ) a )
  -> Array ( HH.HTML w a )
  -> HH.HTML w a
toHeader H1 = HH.h1
toHeader H2 = HH.h2
toHeader H3 = HH.h3
toHeader H4 = HH.h4
toHeader H5 = HH.h5
toHeader H6 = HH.h6

{-----------------------------------------------------------------------}

newtype Language = Language String

derive newtype instance eqLanguage :: Eq Language
derive newtype instance showLanguage :: Show Language
derive instance newtypeLanguage ::  Newtype Language _

{-----------------------------------------------------------------------}
