module Language.GML.Parser.AST (
    Program,
    Result,
    variable,
    expr,
    stmt,
    program,
) where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.List (foldl')
import Data.Text hiding (empty, foldl', map)
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.GML.AST
import Language.GML.Parser.Common
import Language.GML.Parser.Lexer
import Language.GML.Types

-- * Basic tokens

varName = ident <?> "variable"
funName = ident <?> "function"
-- * Values

accessor1 = do
    char '['
    spec <- option '@' $ oneOf ['|', '?', '@']
    spaces
    let cons = case spec of
            '|' -> SList
            '?' -> SMap
            '@' -> SArray
            _ -> error "impossible"
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
            _ -> error "impossible"
    arg1 <- expr
    arg2 <- comma *> expr
    symbol "]"
    return $ \var -> VContainer2 cons var (arg1, arg2)

variable = do
    (var : vars) <- varName `sepBy1` symbol "."
    accs <- many (try accessor1 <|> accessor2)
    let nest = foldl' VField (VVar var) vars
    let nest2 = foldl' (flip ($)) nest accs
    return nest2

function =
    Function
        <$> parens (ident `sepBy` comma)
        <*> choice
            [ colon *> (Constructor . Just <$> funcall) <* kwConstructor
            , kwConstructor $> Constructor Nothing
            , return PlainFunction
            ]
        <*> block

-- * Expressions

opTable :: [[Operator Parser Expr]]
opTable =
    [
        [ prefix "-" UNeg
        , prefix "~" UBitNeg
        , prefix "!" UNot
        -- , prefix "+" id
        ]
    ,
        [ binaryK "div" IntDiv
        , binary "%" Mod
        , binaryK "mod" Mod
        ]
    ,
        [ prefix "--" UPreDec
        , prefix "++" UPreInc
        , postfix "--" UPostDec
        , postfix "++" UPostInc
        ]
    ,
        [ binary "|" BitOr
        , binary "&" BitAnd
        , binary "^" BitXor
        , binary ">>" Shr
        , binary "<<" Shl
        ]
    ,
        [ binary "*" Mul
        , binary "/" Div
        ]
    ,
        [ binary "+" Add
        , binary "-" Sub
        ]
    ,
        [ binary "==" Eq
        , binary "!=" NotEq
        , binary "<=" LessEq
        , binary "<" Less
        , binary ">=" GreaterEq
        , binary ">" Greater
        ]
    ,
        [ binary "??" Nullish
        ]
    ,
        [ binary "&&" And
        , binaryK "and" And
        , binary "||" Or
        , binaryK "or" Or
        , binary "^^" Xor
        , binaryK "xor" Xor
        ]
    ]

binary, binaryK :: Text -> BinOp -> Operator Parser Expr
binary name op = InfixL (EBinary op <$ operator name)
binaryK name op = InfixL (EBinary op <$ keyword name)

prefix, postfix :: Text -> UnOp -> Operator Parser Expr
prefix name op = Prefix (EUnary op <$ operator name)
postfix name op = Postfix (EUnary op <$ operator name)

funcall = (,) <$> funName <*> parens (expr `sepBy` comma)

kwValues :: Parser Expr
kwValues =
    choice
        [ kwUndefined $> EUndefined
        , kwTrue $> EBool True
        , kwFalse $> EBool False
        , kwPointerNull $> EPointer -- FIXME
        , kwPointerInvalid $> EPointer -- FIXME
        , kwPi $> ENumber pi
        ]

eTerm =
    choice
        [ parens expr
        , kwValues
        , ENumber <$> lNumber
        , EString <$> lString
        , EArray <$> brackets (expr `sepBy1` comma)
        , EStruct <$> braces (((,) <$> ident <*> (colon *> expr)) `sepBy` comma)
        , EFunction <$> (kwFunction *> function)
        , ENew <$> (kwNew *> funcall)
        , try (EFuncall <$> funcall)
        , EVariable <$> located variable
        ]

expr :: Parser Expr
expr = makeExprParser eTerm opTable <* spaces <?> "expression"

-- * Statements

sDeclare = SDeclare <$> (kwVar *> (((,) <$> varName <*> optional (symbol "=" *> expr)) `sepBy1` comma))

sEnum = SEnum <$> (kwEnum *> ident) <*> braces (ident `sepBy` comma)

sAssign = do
    var <- located variable
    op <- choice (map (\(c, s) -> c <$ symbol s) ops) <?> "assignment operator"
    op var <$> expr
  where
    ops =
        [ (SAssign, "=")
        , (SAssign, ":=")
        , (SModify MAdd, "+=")
        , (SModify MSub, "-=")
        , (SModify MMul, "*=")
        , (SModify MDiv, "/=")
        , (SModify MBitOr, "|=")
        , (SModify MBitAnd, "&=")
        , (SModify MNullish, "??=")
        ]

sSwitch = do
    kwSwitch
    cond <- expr
    branches <- braces $ some $ do
        cases <-
            some (kwCase *> expr <* colon)
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
stmt =
    ( choice
        [ SBlock <$> block
        , SBreak <$ kwBreak
        , SContinue <$ kwContinue
        , SExit <$ kwExit
        , SFunction <$> (kwFunction *> ident) <*> function
        , sDeclare
        , sEnum
        , SWith <$> (kwWith *> parens expr) <*> stmt
        , SRepeat <$> (kwRepeat *> expr) <*> stmt
        , SWhile <$> (kwWhile *> expr) <*> stmt
        , SDoUntil <$> (kwDo *> stmt) <*> (kwUntil *> expr)
        , SFor <$> (kwFor *> parenL *> forInit <* semicolon) <*> (expr <* semicolon) <*> (forStep <* parenR) <*> stmt
        , SIf <$> (kwIf *> expr) <*> stmt <*> optional (kwElse *> stmt)
        , SReturn <$> (kwReturn *> expr)
        , SThrow <$> (kwThrow *> expr)
        , sSwitch
        , STry <$> (kwTry *> block) <*> optional ((,) <$> (kwCatch *> parens ident) <*> block) <*> optional (kwFinally *> block)
        , try sAssign
        , SExpression <$> expr
        ]
        <?> "statement"
    )
        <* optional semicolon

block :: Parser Block
block = (braceL <|> kwBegin) *> manyTill stmt (braceR <|> kwEnd)

program :: Parser Program
program = manyAll stmt
