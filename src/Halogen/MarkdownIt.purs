module Halogen.MarkdownIt where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Halogen.HTML as HH
import Halogen.MarkdownIt.Utils as MU
import Simple.MarkdownIt as Md
import Text.HTML.Parser as Html
import Text.HTML.Types (Tag(..), Tags)
import Text.Parsing.StringParser (ParseError)


-- | Type synonym for convenience.
type ParentElement w a = Array (HH.HTML w a) -> HH.HTML w a


-- | Creates a Halogen view from `Tags`.
viewFromTags
  :: forall w a
   . Tags
  -> ParentElement w a
  -> Either String (HH.HTML w a)
viewFromTags tags root = tailRec go { tags, contexts: (root /\ Nil) : Nil }
  where
    go { tags: t : ts, contexts } =
      case t of

        TagOpen name attributes ->
          let
            context = (MU.element name (MU.attrs attributes) /\ Nil)
          in
            Loop { tags: ts, contexts: context : contexts }

        TagClose _ ->
          case contexts of
            (parent /\ pElems) : (grandparent /\ gElems) : rest ->
              let
                elem = parent $ Array.reverse $ Array.fromFoldable pElems
                context = grandparent /\ elem : gElems
              in
                Loop { tags: ts, contexts: context : rest }

            _ ->
              Done $ Left $ "could not pop context"

        TagSingle name attributes ->
          case contexts of
            (parent /\ pElems) : rest ->
              let
                elem = MU.element name (MU.attrs attributes) [ ]
                context = parent /\ elem : pElems
              in
                Loop { tags: ts, contexts: context : rest }

            _ -> Done $ Left $ "could not pop context"

        TagText text ->
          case contexts of
            (parent /\ pElems) : rest ->
              let
                context = parent /\ HH.text text : pElems
              in
                Loop { tags: ts, contexts: context : rest }

            _ -> Done $ Left $ "could not pop context"

        TagComment _ ->
          Loop { tags: ts, contexts }

        TagDoctype ->
          Loop { tags: ts, contexts }

    go { tags: Nil, contexts: (parent /\ elements) : Nil } =
      Done $ Right $ parent $ Array.reverse $ Array.fromFoldable elements

    go { tags: _, contexts: _ } = Done $ Left $ "error creating a view"


-- | Creates a Halogen view from a Markdown string.
viewFromMd
  :: forall w a
   . String
  -> ParentElement w a
  -> Effect (Either String (HH.HTML w a))
viewFromMd markdown root =  do
  tags_ <- makeTags
  pure do
    tags <- tags_
    viewFromTags tags root
  where
    makeTags :: Effect (Either String Tags)
    makeTags = simplify <$> do
      Html.parseTags <$> (Md.render markdown =<< Md.defaultMarkdownIt)

    simplify :: Either ParseError Tags -> Either String Tags
    simplify =
      lmap \{ error, pos } -> error <> "at pos: " <> show pos
