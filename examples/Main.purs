module Main where

import Prelude

import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Markdown (parseMarkdown)
import Halogen.Markdown.Transfomer (IPropSpec)
import Halogen.Storybook (Stories, runStorybook, proxy)


foreign import highlightBlocks :: Effect Unit


stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "Halogen Markdown" $ proxy home ]
  where
    home =
      H.mkComponent
        { initialState: identity
        , eval: H.mkEval $ H.defaultEval
          { handleAction = handleAction
          , initialize = Just unit
          }
        , render
        }
      where
        render _ =
          case parseMarkdown props markdown of
            Just html ->
              HH.div_ html
            Nothing ->
              HH.div_
              [ HH.text "Could not parse."
              ]

    handleAction _ = H.liftEffect $ highlightBlocks


css :: forall r a. String -> HP.IProp ( class :: String | r ) a
css = HP.class_ <<< H.ClassName


markdown :: String
markdown = List.intercalate "\n"
  [ "# Hello, World"
  , "Paragraph 1"
  , "Sentence 1"
  , ""
  , "Paragraph 2"
  , "Sentence 2"
  , ""
  , "```py"
  , "# Python codeblock"
  , "print('hello, world!')"
  , "```"
  ]


props :: forall a. IPropSpec a
props =
  { h1:   [ HP.style "font-size: 250%; margin-bottom: 10px;" ]
  , h2:   [ HP.style "font-size: 225%; margin-bottom: 10px;" ]
  , h3:   [ HP.style "font-size: 200%; margin-bottom: 10px;" ]
  , h4:   [ HP.style "font-size: 175%; margin-bottom: 10px;" ]
  , h5:   [ HP.style "font-size: 150%; margin-bottom: 10px;" ]
  , h6:   [ HP.style "font-size: 125%; margin-bottom: 10px;" ]
  , p:    [ HP.style "margin-bottom: 10px;" ]
  , pre:  [ HP.style "background-color: initial; padding: 0;" ]
  , code: [ HP.style "padding: 1.25rem 1.50rem;", css "language-python" ]
  }


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Halogen Markdown" } body
