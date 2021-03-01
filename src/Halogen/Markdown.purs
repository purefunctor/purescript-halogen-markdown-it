module Halogen.Markdown where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.List (List)
import Data.List as List
import Data.String as String
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (char, eof, regex, skipSpaces, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (choice, many)


data MarkdownAST
  = Heading Level String
  | Text String
  | BlankLine
  | Block (List MarkdownAST)

derive instance eqMarkdownAST :: Eq MarkdownAST

instance showMarkdownAST :: Show MarkdownAST where
  show = case _ of
    Heading level text -> List.intercalate " " [ "Heading", show level, text ]
    Text text -> List.intercalate " " [ "Text", text ]
    BlankLine -> "BlankLine"
    Block text -> List.intercalate " " [ "Block", show text ]


newtype Level = Level Int

derive instance eqLevel :: Eq Level
derive instance ordLevel :: Ord Level

instance showLevel :: Show Level where
  show (Level level) = show level


pHeader :: Parser MarkdownAST
pHeader = do
  leading <- String.length <$> whiteSpace

  when (leading > 3) (fail "Invalid heading")

  level <- String.length <$> regex "#+"
  skipSpaces
  title <- regex ".*" <* (many $ char '\n')

  when (level > 6) (fail "Invalid heading")

  pure $ Heading (Level level) title


pBlank :: Parser MarkdownAST
pBlank = char '\n' *> pure BlankLine


pText :: Parser MarkdownAST
pText = Text <$> (skipSpaces *> text <* ((void $ char '\n') <|> eof))
  where
    text = choice $ try <$>
      [ (<>) <$> pure "#" <*> (string "\\#" *> regex ".+")
      , regex ".+"
      ]


pBlock :: Parser MarkdownAST
pBlock = Block <$> (many $ choice $ try <$> [pHeader, pBlank, pText])


parseMarkdown :: String -> Either ParseError MarkdownAST
parseMarkdown = runParser pBlock
