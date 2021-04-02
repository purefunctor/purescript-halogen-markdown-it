module Halogen.Markdown.Parser where

import Prelude

import Control.Alternative ((<|>))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen.Markdown.AST (Language(..), Line(..), Lines, Text(..), toLevel)
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodePoints (char, eof, regex, skipSpaces, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (choice, manyTill)

{-----------------------------------------------------------------------}

textLine :: Parser Line
textLine = TextLine <$> text
  where
    text :: Parser Text
    text = map Text <$> choice $ try <$>
      [ escaped "#"
      , escaped "`"
      , regex ".+"
      ]

    escaped :: String -> Parser String
    escaped c = do
      _ <- string $ "\\" <> c
      t <- regex ".*"
      pure $ c <> t


blankLine :: Parser Line
blankLine = pure BlankLine

{-----------------------------------------------------------------------}

heading :: Parser Line
heading = do
  leading <- String.length <$> whiteSpace

  when ( leading > 3 ) ( fail "Invalid heading" )

  mLevel <- toLevel <<< String.length <$> regex "#+"
  skipSpaces
  title <- regex ".*"

  case mLevel of
    Nothing ->
      fail "Invalid heading"
    Just level ->
      pure $ Heading level ( Text title )

{-----------------------------------------------------------------------}

codeBlock :: Parser Line
codeBlock = do
  _ <- string "```"

  language <- regex ".*" <* char '\n'

  code <- manyTill ( Text <$> regex ".*" <* char '\n' ) ( string "```" )

  pure $
    CodeBlock (Language language) (List.intercalate (Text "\n") code)

{-----------------------------------------------------------------------}

line :: Parser Line
line = choice $ try <$>
  [ heading
  , codeBlock
  , textLine
  , blankLine
  ]

lines :: Parser Lines
lines = manyTill (line <* ending) eof
  where
    ending :: Parser Unit
    ending = ( void $ char '\n' ) <|> eof

{-----------------------------------------------------------------------}
