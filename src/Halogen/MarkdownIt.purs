module Halogen.MarkdownIt
 ( module Halogen.MarkdownIt
 , module Halogen.MarkdownIt.Render
 )
 where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.MarkdownIt.Render (Context(..), applyContext, emptyContext, makeContext, pushContext, viewFromMd, viewFromTags)


type Input =
  { markdown :: String
  }


type State w a =
  { markdown :: String
  , body :: Maybe (Array (HH.HTML w a))
  }


data Action
  = Initialize
  | Receive Input


initialState :: forall w a. Input -> State w a
initialState { markdown } = { markdown, body: Nothing }


render
  :: forall w a
   . (Array (HH.HTML w a) -> HH.HTML w a)
  -> State w a
  -> HH.HTML w a
render root { body } =
  case body of
    Just body' ->
      root body'
    Nothing ->
      HH.div_ [ HH.text "Failed!" ]


makeComponent
  :: forall query slots output m
   . MonadEffect m
  => (Array (H.ComponentHTML Action slots m) -> H.ComponentHTML Action slots m)
  -> H.Component query Input output m
makeComponent root =
  H.mkComponent
  { initialState
  , render: HH.memoized (\p n -> p.markdown == n.markdown) (render root)
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , receive = Just <<< Receive
    }
  }


handleAction
  :: forall w a s o m
   . MonadEffect m
  => Action
  -> H.HalogenM (State w a) Action s o m Unit
handleAction =
  case _ of
    Initialize -> do
      markdown <- H.gets _.markdown
      updateMarkdown markdown
    Receive { markdown } -> do
      updateMarkdown markdown
  where
    updateMarkdown md = do
      html_ <- H.liftEffect $ viewFromMd md
      case html_ of
        Left _ -> pure unit
        Right html -> H.modify_ _ { body = Just html }
