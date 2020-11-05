{-# LANGUAGE OverloadedStrings #-}

module Language.GML.Parser.Common
    ( Parser, Error, Result
    , Name
    , spaces, parens, braces, brackets, comma, semicolon
    , lexeme, symbol, ident
    , parseMany
    ) where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, space1 )
import qualified Text.Megaparsec.Char.Lexer as L

import Language.GML.Types (Name)

type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void
type Result a = Either Error a

{-| Spaces and comments skipper. -}
spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

comma, semicolon :: Parser ()
comma     = symbol "," *> return ()
semicolon = symbol ";" *> return ()

{-| Identifier -}
ident :: Parser Name
ident = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parseMany :: Parser a -> String -> Text -> Result [a]
parseMany p = parse (spaces *> many p <* eof)
