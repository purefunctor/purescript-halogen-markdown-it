module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.MarkdownIt as HM
import Halogen.Storybook (Stories, proxy, runStorybook)


foreign import highlightBlocks :: Effect Unit


stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "Halogen Markdown" $ proxy home ]
  where
    home =
      H.mkComponent
        { initialState: \_ -> HH.div [ ] [ HH.text "Loading..." ]
        , eval: H.mkEval $ H.defaultEval
          { handleAction = handleAction
          , initialize = Just unit
          }
        , render
        }
      where
        render = identity

    handleAction _ = do
      H.liftEffect $ highlightBlocks

      html_ <- H.liftEffect $
        HM.viewFromMd markdown HH.article_

      H.put $ case html_ of
        Left e -> HH.h1_ [ HH.text e ]
        Right h -> h


css :: forall r a. String -> HP.IProp ( class :: String | r ) a
css = HP.class_ <<< H.ClassName


markdown :: String
markdown = """
# Hello, World
This is a paragraph for example.
"""


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Halogen Markdown" } body
