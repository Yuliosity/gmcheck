module Language.GML.Parser.AST
    ( Program, Result
    , variable, expr, stmt, program
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.List (foldl')
import Data.Text hiding (foldl', empty, map)

import Language.GML.AST
import Language.GML.Types
import Language.GML.Parser.Common
import Language.GML.Parser.Lexer

-- * Basic tokens

varName = ident <?> "variable"
funName = ident <?> "function"

funArgs = parens $ ident `sepBy` comma

-- * Values

accessor1 = do
    char '['
    spec <- option '@' $ oneOf [ '|', '?', '@' ]
    spaces
    let cons = case spec of
            '|' -> SList
            '?' -> SMap
            '@' -> SArray
    arg <- expr
    symbol "]"
    return $ \var -> VContainer cons var arg

accessor2 = do
    char '['
    spec <- option ' ' $ char '#'
    spaces
    let cons = case spec of
            ' ' -> SArray2
            '#' -> SGrid
    arg1 <- expr
    arg2 <- comma *> expr
    symbol "]"
    return $ \var -> VContainer2 cons var (arg1, arg2)

variable = do
    (var:vars) <- varName `sepBy1` symbol "."
    accs <- many (try accessor1 <|> accessor2)
    let nest  = foldl' VField (VVar var) vars
    let nest2 = foldl' (flip ($)) nest accs
    return nest2

-- * Expressions

funcall = EFuncall <$> funName <*> parens (expr `sepBy` comma)

opTable :: [[Operator Parser Expr]]
opTable =
    [   [ prefix "-" UNeg
        , prefix "~" UBitNeg
        , prefix "!" UNot
        --, prefix "+" id
        ]
    ,   [ binaryK "div" IntDiv
        , binary  "%"   Mod
        , binaryK "mod" Mod
        ]
    ,   [ prefix  "--" UPreDec
        , prefix  "++" UPreInc
        , postfix "--" UPostDec
        , postfix "++" UPostInc
        ]
    ,   [ binary "|"  BitOr
        , binary "&"  BitAnd
        , binary "^"  BitXor
        , binary ">>" Shr
        , binary "<<" Shl
        ]
    ,   [ binary "*"  Mul
        , binary "/"  Div
        ]
    ,   [ binary "+"  Add
        , binary "-"  Sub
        ]
    ,   [ binary "==" Eq
        , binary "!=" NotEq
        , binary "<=" LessEq
        , binary "<"  Less
        , binary ">=" GreaterEq
        , binary ">"  Greater
        ]
    ,   [ binary  "&&"  And
        , binaryK "and" And
        , binary  "||"  Or
        , binaryK "or"  Or
        , binary  "^^"  Xor
        , binaryK "xor" Xor
        ]
    ]

binary, binaryK :: Binary a => Text -> a -> Operator Parser Expr
binary  name op = InfixL  (eBinary op <$ operator name)
binaryK name op = InfixL  (eBinary op <$ keyword name)

prefix, postfix :: Text -> UnOp -> Operator Parser Expr
prefix  name op = Prefix  (EUnary op <$ operator name)
postfix name op = Postfix (EUnary op <$ operator name)

eTerm = choice
    [ parens expr
    , EArray <$> brackets (expr `sepBy1` comma)
    , ENumber <$> lNumber
    , EString <$> lString
    , EFunction <$> (kwFunction *> funArgs) <*> block
    , try funcall
    , EVariable <$> variable
    ]

expr :: Parser Expr
expr = makeExprParser eTerm opTable <* spaces <?> "expression"

-- * Statements

sDeclare = SDeclare <$> (kwVar *> (((,) <$> varName <*> optional (symbol "=" *> expr)) `sepBy1` comma))

sAssign = do
    var <- variable
    op <- choice (map (\(c, s) -> c <$ symbol s) ops) <?> "assignment operator" 
    op var <$> expr
    where
        ops =
            [ (SAssign, "="), (SAssign, ":=")
            , (SModify Add, "+="), (SModify Sub, "-=")
            , (SModify Mul, "*="), (SModify Div, "/=")
            , (SModify BitOr, "|="), (SModify BitAnd, "&="), (SModify BitXor, "^=")
            ]

sSwitch = do
    kwSwitch
    cond <- expr
    branches <- braces $ some $ do
        cases <- some (kwCase *> expr <* colon)
            <|> (kwDefault *> colon $> [])
        body <- many stmt
        optional semicolon
        return (cases, body)
    return $ SSwitch cond branches 

forInit :: Parser Stmt
forInit = sDeclare <|> sAssign

forStep :: Parser Stmt
forStep = try sAssign <|> SExpression <$> expr

-- | A single statement, optionally ended with a semicolon.
stmt :: Parser Stmt
stmt = (choice
    [ SBlock        <$> block
    , SBreak <$ kwBreak, SContinue <$ kwContinue, SExit <$ kwExit
    , SFunction     <$> (kwFunction *> ident) <*> funArgs <*> block
    , sDeclare
    , SWith         <$> (kwWith *> parens expr) <*> stmt
    , SRepeat       <$> (kwRepeat *> expr) <*> stmt
    , SWhile        <$> (kwWhile  *> expr) <*> stmt
    , SDoUntil      <$> (kwDo *> stmt) <*> (kwUntil *> expr)
    , SFor          <$> (kwFor *> parenL *> forInit <* semicolon) <*> (expr <* semicolon) <*> (forStep <* parenR) <*> stmt
    , SIf           <$> (kwIf *> expr) <*> stmt <*> optional (kwElse *> stmt)
    , SReturn       <$> (kwReturn *> expr)
    , sSwitch
    , try sAssign
    , SExpression   <$> expr
    ] <?> "statement")
    <* optional semicolon

block :: Parser Block
block = (braceL <|> kwBegin) *> manyTill stmt (braceR <|> kwEnd)

program :: Parser Program
program = manyAll stmt
