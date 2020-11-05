{-# LANGUAGE OverloadedStrings #-}

module Language.GML.Parser.AST
    ( Program, Result
    , variable, expr, stmt, program
    , parseProgram
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Text hiding (empty, map)

import Language.GML.AST
import Language.GML.Types
import Language.GML.Parser.Common

-- * Basic tokens

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy alphaNumChar)

varName = ident <?> "variable"
funName = ident <?> "function or script"

-- * Values

-- |Number literal.
lNumeric :: Parser Literal
lNumeric = LNumeric <$>
    (try (lexeme (L.signed empty L.float))
    <|> fromIntegral <$> lexeme (L.signed empty L.decimal))

-- |String literal.
lString :: Parser Literal
lString = LString <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

literal = lNumeric <|> lString

accessor1 name = do
    char '['
    spec <- optional $ oneOf ['|', '?', '@']
    spaces
    let cons = case spec of
            Nothing -> SArray
            Just c -> case c of
                '|' -> SList
                '?' -> SMap
                '@' -> SArray
    arg <- expr
    symbol "]"
    return $ VContainer cons name arg

accessor2 name = do
    char '['
    spec <- optional $ char '#'
    spaces
    let cons = case spec of
            Nothing -> SArray2
            Just c -> case c of
                '#' -> SGrid
    arg1 <- expr
    arg2 <- comma *> expr
    symbol "]"
    return $ VContainer2 cons name (arg1, arg2)


variable = do
    name <- varName
    choice
        [ VField name <$> (symbol "." *> variable)
        , try $ accessor1 name
        , accessor2 name
        , pure $ VVar name
        ]

-- * Expressions

funcall = EFuncall <$> funName <*> parens (expr `sepBy` comma)

opTable :: [[Operator Parser Expr]]
opTable =
    [   [ prefix "-" (EUnary UNeg)
        , prefix "~" (EUnary UBitNeg)
        , prefix "!" (EUnary UNot)
        , prefix "+" id
        ]
    ,   [ binary "div" (eBinary IntDiv)
        , binary "%"   (eBinary Mod)
        , binary "mod" (eBinary Mod)
        ]
    ,   [ prefix  "--" (EUnary UPreDec)
        , prefix  "++" (EUnary UPreInc)
        , postfix "--" (EUnary UPostDec)
        , postfix "++" (EUnary UPostInc)
        ]
    ,   [ binary "|"  (eBinary BitOr)
        , binary "&"  (eBinary BitAnd)
        , binary "^"  (eBinary BitXor)
        , binary ">>" (eBinary Shr)
        , binary "<<" (eBinary Shl)
        ]
    ,   [ binary "*"  (eBinary Mul)
        , binary "/"  (eBinary Div)
        ]
    ,   [ binary "+"  (eBinary Add)
        , binary "-"  (eBinary Sub)
        ]
    ,   [ binary "<"  (eBinary Less)
        , binary "==" (eBinary Eq)
        , binary "!=" (eBinary NotEq)
        , binary ">"  (eBinary Greater)
        , binary "<=" (eBinary LessEq)
        , binary ">=" (eBinary GreaterEq)
        ]
    ,   [ binary "&&" (eBinary And)
        , binary "||" (eBinary Or)
        , binary "^^" (eBinary Xor)
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

eTerm = choice
    [ parens expr
    , ELiteral <$> literal
    , try funcall
    , EVariable <$> variable
    ]

expr :: Parser Expr
expr = makeExprParser eTerm opTable <?> "expression"

assignOp = choice (map (\(c, s) -> c <$ symbol s) ops) <?> "assignment" where
    ops =
        [ (AAssign, "="), (AAssign, ":=")
        , (AModify Add, "+="), (AModify Sub, "-=")
        , (AModify Mul, "*="), (AModify Div, "/=")
        , (AModify Or,  "|="), (AModify And, "&="), (AModify Xor, "^=")
        ]

-- * Statements

-- | A single statement, optionally ended with a semicolon.
stmt :: Parser Stmt
stmt = (choice
    [ SBlock        <$> ((symbol "{" <|> keyword "begin") *> manyTill stmt (symbol "}" <|> keyword "end"))
    , SBreak <$ keyword "break", SContinue <$ keyword "continue", SExit <$ keyword "exit"
    , SDeclare      <$> (keyword "var" *> ((,) <$> varName <*> optional (assignOp *> expr)) `sepBy1` comma)
    , SWith         <$> (keyword "with" *> parens expr) <*> stmt
    , SRepeat       <$> (keyword "repeat" *> expr) <*> stmt
    , SWhile        <$> (keyword "while"  *> expr) <*> stmt
    , SDoUntil      <$> (keyword "do" *> stmt) <*> (keyword "until" *> expr)
    , SIf           <$> (keyword "if" *> expr) <*> stmt <*> optional (keyword "else" *> stmt)
    , SReturn       <$> (keyword "return" *> expr)
    , try $ SAssign <$> variable <*> assignOp <*> expr
    , SExpression   <$> expr
    ] <?> "statement")
    <* optional semicolon

program :: Parser Program
program = many stmt

parseProgram :: String -> Text -> Result Program
parseProgram = parseMany stmt
