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
import Data.List (foldl')
import Data.Text hiding (foldl', empty, map)

import Language.GML.AST
import Language.GML.Types
import Language.GML.Parser.Common

-- * Basic tokens

varName = ident <?> "variable"
funName = ident <?> "function"

-- * Values

-- |Number literal.
lNumeric :: Parser Literal
lNumeric = LNumeric <$>
    (try (lexeme (L.signed empty L.float))
    <|> fromIntegral <$> lexeme (L.signed empty L.decimal))

-- |String literal.
lString :: Parser Literal
lString = LString <$> (char '\"' *> manyTill L.charLiteral (char '\"') <* spaces)

literal = lNumeric <|> lString

accessor1 = do
    char '['
    spec <- optional $ oneOf [ '|', '?', '@' ]
    spaces
    let cons = case spec of
            Nothing -> SArray
            Just c -> case c of
                '|' -> SList
                '?' -> SMap
                '@' -> SArray
    arg <- expr
    symbol "]"
    return $ \var -> VContainer cons var arg

accessor2 = do
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
    return $ \var -> VContainer2 cons var (arg1, arg2)

variable = do
    (var:vars) <- varName `sepBy1` (symbol ".")
    accs <- many (try accessor1 <|> accessor2)
    let nest  = foldl' VField (VVar var) vars
    let nest2 = foldl' (flip ($)) nest accs
    return nest2

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
    ,   [ binary "==" (eBinary Eq)
        , binary "!=" (eBinary NotEq)
        , binary "<=" (eBinary LessEq)
        , binary "<"  (eBinary Less)
        , binary ">=" (eBinary GreaterEq)
        , binary ">"  (eBinary Greater)
        ]
    ,   [ binary "&&" (eBinary And)
        , binary "||" (eBinary Or)
        , binary "^^" (eBinary Xor)
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ operator name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ operator name)
postfix name f = Postfix (f <$ operator name)

eTerm = choice
    [ parens expr
    , ELiteral <$> literal
    , try funcall
    , EVariable <$> variable
    ]

expr :: Parser Expr
expr = makeExprParser eTerm opTable <* spaces <?> "expression"

assignOp = choice (map (\(c, s) -> c <$ symbol s) ops) <?> "assignment" where
    ops =
        [ (AAssign, "="), (AAssign, ":=")
        , (AModify Add, "+="), (AModify Sub, "-=")
        , (AModify Mul, "*="), (AModify Div, "/=")
        , (AModify BitOr, "|="), (AModify BitAnd, "&="), (AModify BitXor, "^=")
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
    , SFor          <$> (keyword "for" *> symbol "(" *> stmt) <*> (expr <* semicolon) <*> (stmt <* symbol ")") <*> stmt
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
