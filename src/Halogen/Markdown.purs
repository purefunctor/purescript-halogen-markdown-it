module Halogen.Markdown where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.Markdown.Parser (lines)
import Halogen.Markdown.Transfomer (toHalogen)
import Text.Parsing.StringParser (runParser)

{-----------------------------------------------------------------------}

parseMarkdown :: forall w a. String -> Maybe ( Array ( HH.HTML w a ) )
parseMarkdown = map toHalogen <<< hush <<< runParser lines

{-----------------------------------------------------------------------}
