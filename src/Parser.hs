{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text
import Data.Void (Void)

import AST

type Parser = Parsec Void Text

{-| Spaces and comments skipper. -}
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

{-| Number literal.-}
numericLiteral :: Parser Double
numericLiteral = lexeme L.float

{-| String literal.-}
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

literal
    = LNumeric <$> numericLiteral
    <|> LString <$> stringLiteral

sVar = SVar <$> (string "var" *> varName) <*> (string "=" *> expression)

varName = (:) <$> letterChar <*> many alphaNumChar

statement = sVar

expression = choice [eBinary, ELiteral <$> literal]

binOp = choice [op "+" BAdd, op "<" BLess, op "==" BEq] where
    op str res = string str *> pure res

eBinary = EBinary <$> expression <*> binOp <*> expression
