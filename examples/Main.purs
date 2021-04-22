module Main where

import Prelude

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
import Type.Proxy (Proxy(..))


foreign import highlightBlocks :: Effect Unit


stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "Halogen Markdown" $ proxy home ]
  where
    home =
      H.mkComponent
        { initialState: \_ -> unit
        , eval: H.mkEval $ H.defaultEval
          { handleAction = handleAction
          , initialize = Just unit
          }
        , render
        }
      where
        render _ = HH.div_
          [ HH.slot ( Proxy :: _ "markdown" ) unit component { markdown } absurd
          ]

        component = HM.makeComponent HH.article_

    handleAction _ =
      H.liftEffect $ highlightBlocks


css :: forall r a. String -> HP.IProp ( class :: String | r ) a
css = HP.class_ <<< H.ClassName


markdown :: String
markdown = """
# Hello, World
This is a paragraph for example.

* Group 1
  + Item 1
  + Item 2
* Group 2
  + Item 1
  + Item 2

```py
print('hello, world')
```
"""


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Halogen Markdown" } body
