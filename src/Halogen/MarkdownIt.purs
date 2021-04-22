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
        (TagOpen _ _) ->
          Loop { tags: ts, contexts: (HH.div_ /\ Nil) : contexts }
        (TagClose _) ->
          case contexts of
            (xp /\ xe) : (yp /\ ye) : zs -> do
              let x = xp $ Array.reverse $ Array.fromFoldable $ xe
                  y = yp /\ (x : ye)
              Loop { tags: ts, contexts: y : zs }
            _ -> Done $ Left $ "could not close tag"
        (TagSingle _ _) ->
          case contexts of
            (xp /\ xe) : zs ->
              Loop { tags: ts, contexts: (xp /\ (HH.div_ [ ] : xe)) : zs }
            _ -> Done $ Left $ "could not pop context"
        (TagText n) ->
          case contexts of
            (xp /\ xe) : zs ->
              Loop { tags: ts, contexts: (xp /\ (HH.text n : xe)) : zs }
            _ -> Done $ Left $ "could not pop context"
        (TagComment _) ->
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
