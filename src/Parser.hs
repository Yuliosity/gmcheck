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

assignOp = symbol "=" <|> symbol ":="

sDeclare = SDeclare <$> (symbol "var" *> varName) <*> optional (assignOp *> expr)

sAssign = SAssign <$> variable <*> (assignOp *> expr)

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

variable = choice
    [ VVar <$> varName
    , VField <$> varName <*> (symbol "." *> variable)
    , VArray <$> variable <*> brackets expr
    , VArray2 <$> variable <*> brackets ((,) <$> expr <*> (symbol "," *> expr))
    ]

varName = (:) <$> letterChar <*> many alphaNumChar <?> "variable"

stmt :: Parser Stmt
stmt = sDeclare

opTable :: [[Operator Parser Expr]]
opTable =
    [   [ prefix "-" (EUnary UNeg)
        , prefix "~" (EUnary UBitNeg)
        , prefix "!" (EUnary UNot)
        , prefix "+" id
        ]
    ,   [ binary "div" (EBinary BIntDiv)
        , binary "%" (EBinary BMod)
        , binary "mod" (EBinary BMod)
        ]
    ,   [ prefix "--" (EUnary UPreDec)
        , prefix "++" (EUnary UPreInc)
        , postfix "--" (EUnary UPostDec)
        , postfix "++" (EUnary UPostInc)
        ]
    ,   [ binary "|" (EBinary BBitOr)
        , binary "&" (EBinary BBitAnd)
        , binary "^" (EBinary BBitXor)
        , binary ">>" (EBinary BShr)
        , binary "<<" (EBinary BShl)
        ]
    ,   [ binary "*" (EBinary BMul)
        , binary "/" (EBinary BDiv)
        ]
    ,   [ binary "+" (EBinary BAdd)
        , binary "-" (EBinary BSub)
        ]
    ,   [ binary "<" (EBinary BLess)
        , binary "==" (EBinary BEq)
        , binary "!=" (EBinary BNotEq)
        , binary ">" (EBinary BGreater)
        , binary "<=" (EBinary BLessEq)
        , binary ">=" (EBinary BGreaterEq)
        ]
    ,   [ binary "&&" (EBinary BAnd)
        , binary "||" (EBinary BOr)
        , binary "^^" (EBinary BXor)
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

eTerm = choice [ELit <$> literal, EVar <$> variable]

expr :: Parser Expr
expr = makeExprParser eTerm opTable
