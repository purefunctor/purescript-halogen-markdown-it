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
import Halogen.Markdown (parseMarkdown)
import Halogen.Storybook (Stories, runStorybook, proxy)

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

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "Halogen Markdown" $ proxy home ]
  where
    home =
      H.mkComponent
        { initialState: identity
        , eval: H.mkEval H.defaultEval
        , render
        }
      where
        render _ =
          case parseMarkdown markdown of
            Just html ->
              HH.div_ html
            Nothing ->
              HH.div_
              [ HH.text "Could not parse."
              ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Halogen Markdown" } body
