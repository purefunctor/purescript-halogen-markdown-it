module Halogen.MarkdownIt.Render where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.HTML as HH
import Halogen.MarkdownIt.Utils as MU
import Simple.MarkdownIt as Md
import Text.HTML.Parser as Html
import Text.HTML.Types (Attributes, Name, Tag(..), Tags)
import Text.Parsing.StringParser (ParseError)


-- | Represents the "current" frame in the stack of open tags.
newtype Context w a = Context
  { root :: Maybe (Array (HH.HTML w a) -> HH.HTML w a)
  , items :: List (HH.HTML w a )
  }


-- | Creates a `Context` from a `Name` and `Attributes`.
makeContext :: forall w a. Name -> Attributes -> Context w a
makeContext name attributes = Context
  { root: Just $ MU.makeRoot name attributes, items: Nil }


-- | A `Context` with no `root` or `items`.
emptyContext :: forall w a. Context w a
emptyContext = Context { root: Nothing, items: Nil }


-- | Applies a `Context` into its final representation.
applyContext :: forall w a. Context w a -> Maybe (HH.HTML w a)
applyContext (Context context) = context.root <*> items
  where
    items = pure $ Array.reverse $ Array.fromFoldable $ context.items


-- | Pushes an item into a `Context`.
pushContext :: forall w a. HH.HTML w a -> Context w a -> Context w a
pushContext item (Context { root, items }) = Context
  { root, items: item : items }


-- | Creates a Halogen view from `Tags`.
viewFromTags
  :: forall w a
   . Tags
  -> Either String (Array (HH.HTML w a))
viewFromTags tags = tailRec go { tags, contexts: emptyContext : Nil }
  where
    go { tags: tHead : tRest, contexts } =
      case tHead of

        TagOpen name attributes ->
          Loop { tags: tRest, contexts: makeContext name attributes : contexts }

        TagClose _ ->
          case contexts of
            t : u : rest ->

              case applyContext t of
                Just item ->
                  Loop { tags: tRest, contexts: pushContext item u : rest }

                Nothing ->
                  Done $ Left "could not apply context"

            _ -> Done $ Left "could not pop context"

        TagSingle name attributes ->
          case contexts of
            t : rest ->
              let
                item = MU.makeRoot name attributes [ ]
              in
                Loop { tags: tRest, contexts: pushContext item t : rest }

            _ -> Done $ Left "could not pop context"

        TagText text ->
          case contexts of
            t : rest ->
              let
                item = HH.text text
              in
                Loop { tags: tRest, contexts: pushContext item t : rest }

            _ -> Done $ Left "could not pop context"

        TagComment _ -> Loop { tags: tRest, contexts }

        TagDoctype -> Loop { tags: tRest, contexts }

    go { tags: Nil, contexts: (Context context) : Nil } =
      Done $ Right $ Array.reverse $ Array.fromFoldable $ context.items

    go { tags: _, contexts: _ } = Done $ Left $ "failure!"


-- | Creates a Halogen view from a Markdown string.
viewFromMd
  :: forall w a
   . String
  -> Effect (Either String (Array (HH.HTML w a)))
viewFromMd markdown =  do
  tags_ <- makeTags
  pure do
    tags <- tags_
    viewFromTags tags
  where
    makeTags :: Effect (Either String Tags)
    makeTags = simplify <$> do
      Html.parseTags <$> (Md.render markdown =<< Md.defaultMarkdownIt)

    simplify :: Either ParseError Tags -> Either String Tags
    simplify =
      lmap \{ error, pos } -> error <> "at pos: " <> show pos
