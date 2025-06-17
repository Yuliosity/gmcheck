module Language.GML.Parser.AST
    ( Program, Result
    , variable, expr, stmt, program
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.List (foldl')
import Data.Text hiding (foldl', empty, map)

import Language.GML.AST
import Language.GML.Types
import Language.GML.Parser.Common
import Language.GML.Parser.Lexer
import Language.GML.Parser.Types (type_)

-- * Basic tokens

varName = ident <?> "variable"
funName = ident <?> "function"
-- * Values

accessor1 = do
    char '['
    spec <- option '@' $ oneOf [ '|', '?', '@' ]
    spaces
    let cons = case spec of
            '|' -> SList
            '?' -> SMap
            '@' -> SArray
            _   -> error "impossible"
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
            _   -> error "impossible"
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

function = Function
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
    ,   [ binary  "??"  Nullish
        ]
    ,   [ binary  "&&"  And
        , binaryK "and" And
        , binary  "||"  Or
        , binaryK "or"  Or
        , binary  "^^"  Xor
        , binaryK "xor" Xor
        ]
    ]

binary, binaryK :: Text -> BinOp -> Operator Parser Expr
binary  name op = InfixL  (EBinary op <$ operator name)
binaryK name op = InfixL  (EBinary op <$ keyword name)

prefix, postfix :: Text -> UnOp -> Operator Parser Expr
prefix  name op = Prefix  (EUnary op <$ operator name)
postfix name op = Postfix (EUnary op <$ operator name)

funcall :: Parser (Text, [Expr])
funcall = (,) <$> funName <*> parens (expr `sepBy` comma)

kwConstants :: Parser Expr
kwConstants = choice
    [ kwUndefined $> EUndefined
    , kwTrue $> EBool True
    , kwFalse $> EBool False
    , kwPointerNull $> EPointer --FIXME
    , kwPointerInvalid $> EPointer --FIXME
    , kwPi $> ENumber pi
    , kwSelf $> EInstance ISelf
    , kwOther $> EInstance IOther
    , kwNoone $> EInstance INoone
    ]

eTerm :: Parser Expr
eTerm = choice
    [ parens expr
    , kwConstants
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

sVarDecl :: Parser VarDecl
sVarDecl = do
    name <- ident_ <* space
    mType <- optional $ inPragma type_
    mInit <- optional $ symbol "=" *> expr
    return $ VarDecl name mInit mType

sDeclare :: Parser Stmt
sDeclare = do
    kwVar   
    vars <- sVarDecl `sepBy1` comma
    return $ SDeclare vars

sEnum :: Parser Stmt
sEnum = SEnum <$> (kwEnum *> ident) <*> braces (ident `sepBy` comma)

sAssign :: Parser Stmt
sAssign = do
    var <- located variable
    op <- choice (map (\(c, s) -> c <$ symbol s) ops) <?> "assignment operator" 
    op var <$> expr
    where
        ops =
            [ (SAssign, "="), (SAssign, ":=")
            , (SModify MAdd, "+="), (SModify MSub, "-=")
            , (SModify MMul, "*="), (SModify MDiv, "/=")
            , (SModify MBitOr, "|="), (SModify MBitAnd, "&=")
            , (SModify MNullish, "??=")
            ]

sSwitch :: Parser Stmt
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

sFor :: Parser Stmt
sFor = do
    kwFor <* parenL
    init <- (sDeclare <|> sAssign) <* semicolon
    cond <- expr <* semicolon
    step <- try sAssign <|> SExpression <$> expr
    parenR
    body <- stmt
    return $ SFor init cond step body

sTry :: Parser Stmt
sTry = do
    kwTry
    body <- block
    catch <- optional ((,) <$> (kwCatch *> parens ident) <*> block)
    finally <- optional (kwFinally *> block)
    return $ STry body catch finally

-- | A single statement, optionally ended with a semicolon.
stmt :: Parser Stmt
stmt = (choice
    [ SBlock      <$> block
    , SBreak <$ kwBreak, SContinue <$ kwContinue, SExit <$ kwExit
    , SFunction   <$> (kwFunction *> ident) <*> function
    , sDeclare
    , sEnum
    , SWith       <$> (kwWith *> parens expr) <*> stmt
    , SRepeat     <$> (kwRepeat *> expr) <*> stmt
    , SWhile      <$> (kwWhile  *> expr) <*> stmt
    , SDoUntil    <$> (kwDo *> stmt) <*> (kwUntil *> expr)
    , sFor
    , SIf         <$> (kwIf *> expr) <*> stmt <*> optional (kwElse *> stmt)
    , SReturn     <$> (kwReturn *> expr)
    , SThrow      <$> (kwThrow  *> expr)
    , sSwitch
    , sTry
    , try sAssign
    , SExpression <$> expr
    ] <?> "statement")
    <* optional semicolon

block :: Parser Block
block = (braceL <|> kwBegin) *> manyTill stmt (braceR <|> kwEnd)

program :: Parser Program
program = manyAll stmt
