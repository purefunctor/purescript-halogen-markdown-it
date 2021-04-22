module Halogen.MarkdownIt.Utils where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.HTML.Types (Attribute(..), Name(..), Value(..))


element
  :: forall r w a
   . Name
  -> Array (HP.IProp r a)
  -> Array (HH.HTML w a)
  -> HH.HTML w a
element (Name name) = HH.element (HH.ElemName name)


attr
  :: forall r a
   . Attribute
  -> HP.IProp r a
attr (Attribute (Name name) (Value value)) = HH.attr (HH.AttrName name) value


attrs
  :: forall f w a
   . Foldable f
  => Functor f
  => f Attribute
  -> Array (HP.IProp w a)
attrs = Array.fromFoldable <<< map attr
