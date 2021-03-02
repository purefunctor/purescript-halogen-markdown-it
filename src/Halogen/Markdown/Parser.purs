module Halogen.Markdown.Parser where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen.Markdown.AST (Markdown(..), toLevel)
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (char, eof, regex, skipSpaces, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (choice, many)


pText ∷ Parser Markdown
pText = Text <$> (skipSpaces *> text <* ((void $ char '\n') <|> eof))
  where
    text ∷ Parser String
    text = choice $ try <$>
      [ escaped "#"
      , regex ".+"
      ]

    escaped ∷ String → Parser String
    escaped c = do
      _ <- string $ "\\" <> c
      t <- regex ".+"
      pure $ c <> t


pHeader ∷ Parser Markdown
pHeader = do
  leading <- String.length <$> whiteSpace

  when (leading > 3) (fail "Invalid heading")

  mLevel <- toLevel <<< String.length <$> regex "#+"
  skipSpaces
  title <- regex ".*" <* (many $ char '\n')

  case mLevel of
    Just level ->
      pure $ Heading level title
    Nothing ->
      fail "Invalid heading"


pBlank ∷ Parser Markdown
pBlank = char '\n' *> pure BlankLine


parseMarkdown ∷ String → Either ParseError (List Markdown)
parseMarkdown = runParser $ many $ choice $ try <$>
  [ pText
  , pHeader
  , pBlank
  ]
