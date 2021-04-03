module Halogen.Markdown where

import Prelude

import Data.Array as Array
import Data.Either (hush)
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.Markdown.Parser (lines)
import Halogen.Markdown.Transfomer (normalize, toHalogen)
import Text.Parsing.StringParser (runParser)

{-----------------------------------------------------------------------}

parseMarkdown :: forall w a. String -> Maybe ( Array ( HH.HTML w a ) )
parseMarkdown = transform <=< hush <<< runParser lines
  where
    transform = pure <<< Array.reverse <<< toHalogen <<< normalize

{-----------------------------------------------------------------------}
