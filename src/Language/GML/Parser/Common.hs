{-# LANGUAGE OverloadedStrings #-}

module Language.GML.Parser.Common
    ( Parser, Error, Result
    , Name
    , parens, braces, brackets, semicolon
    , lexeme, symbol, ident
    , parseMany
    ) where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, space1 )
import qualified Text.Megaparsec.Char.Lexer as L

import Language.GML.AST (Name)

type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void
type Result a = Either Error a

{-| Spaces and comments skipper. -}
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser ()
semicolon = symbol ";" *> return ()

{-| Identifier -}
ident :: Parser Name
ident = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parseMany :: Parser a -> String -> Text -> Result [a]
parseMany p = parse (sc *> many p <* eof)
