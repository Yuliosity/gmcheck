{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
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
numericLiteral =
    lexeme (L.signed sc L.float)
    <|> fromIntegral <$> lexeme (L.signed sc L.decimal)

{-| String literal.-}
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

literal =
    LNumeric <$> numericLiteral
    <|> LString <$> stringLiteral

sVar = SVar <$> (string "var" *> varName) <*> (string "=" *> expr)

varName = (:) <$> letterChar <*> many alphaNumChar <?> "variable"

statement :: Parser Stmt
statement = sVar

opTable :: [[Operator Parser Expr]]
opTable =
    [   [ prefix "-" (EUnary UNeg)
        , prefix "--" (EUnary UPreDec)
        , prefix "++" (EUnary UPostDec)
        , prefix "+" id
        ]
    ,   [ binary "*" (EBinary BMul)
        , binary "/" (EBinary BDiv)
        ]
    ,   [ binary "+" (EBinary BAdd)
        , binary "-" (EBinary BSub)
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

variable = VVar <$> varName

eTerm = choice [ELit <$> literal, EVar <$> variable]

expr :: Parser Expr
expr = makeExprParser eTerm opTable
