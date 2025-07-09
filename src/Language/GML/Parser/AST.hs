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

-- * Variables

varName = choice
    [ kwSelf $> ISelf
    , kwOther $> IOther
    , kwNoone $> INoone
    , VVar <$> ident
    ] <?> "variable"

field = do
    char '.'
    name <- ident
    return $ \var -> VField var name

accessor1 = do
    char '['
    spec <- option '@' $ oneOf [ '|', '?', '@', '$' ]
    spaces
    cons <- case spec of
        '|' -> return SList
        '?' -> return SMap
        '@' -> return SArray
        '$' -> return SStruct
        c   -> fail $ "Unknown accessor " <> show c
    arg <- expr
    symbol "]"
    return $ \var -> VContainer cons var arg

accessor2 = do
    char '['
    spec <- option ' ' $ char '#'
    spaces
    cons <- case spec of
        ' ' -> return SArray2
        '#' -> return SGrid
        c   -> fail $ "Unknown accessor " <> show c
    arg1 <- expr <* comma
    arg2 <- expr
    symbol "]"
    return $ \var -> VContainer2 cons var (arg1, arg2)

variable = do
    var <- varName
    accs <- many (choice [field, try accessor1, accessor2])
    return $ foldl' (flip ($)) var accs

function = Function
    <$> parens (varDecl `sepBy` comma)
    <*> choice
        [ colon *> (Constructor . Just <$> funcall) <* kwConstructor
        , kwConstructor $> Constructor Nothing
        , return PlainFunction
        ]
    <*> block
    where 
        funcall = (,) <$> ident <*> parens (expr `sepBy` comma)

-- * Expressions

opTable :: [[Operator Parser Expr]]
opTable =
    [   [ prefix "-" UNeg
        , prefix "~" UBitNeg
        , prefix "!" UNot
        , prefix "+" UPos
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
    ,   [ ternary
        ]
    ]

-- TODO: refactor the copy-pasta
binary, binaryK :: Text -> BinOp -> Operator Parser Expr
binary  name op = InfixL $ do --  (EBinary op <$ operator name)
    operator name
    return $ \a b -> EBinary op a b :@ getPos a
binaryK name op = InfixL $ do --  (EBinary op <$ keyword name)
    keyword name
    return $ \a b -> EBinary op a b :@ getPos a

prefix, postfix :: Text -> UnOp -> Operator Parser Expr
prefix  name op = Prefix $ do --  (EUnary op <$ operator name)
    _ :@ p <- located (operator name)
    return $ \e -> EUnary op e :@ p
postfix name op = Postfix $ do -- (EUnary op <$ operator name)
    _ :@ p <- located (operator name)
    return $ \e -> EUnary op e :@ p

ternary = TernR do 
    (f <$ symbol ":") <$ symbol "?"
    where
        f e1 e2 e3 = ETernary e1 e2 e3 :@ getPos e1

funcall :: Parser (Variable, [Expr])
funcall = (,) <$> variable <*> parens (expr `sepBy` comma)

kwConstants :: Parser Expr
kwConstants = located $ choice
    [ kwUndefined $> EUndefined
    , kwTrue $> EBool True
    , kwFalse $> EBool False
    , kwPointerNull $> EPointer --FIXME
    , kwPointerInvalid $> EPointer --FIXME
    , kwPi $> ENumber pi
    ]

eTerm :: Parser Expr
eTerm = located $ choice
    [ unLoc <$> parens expr
    , unLoc <$> kwConstants
    , ENumber <$> lNumber
    , EString <$> lString
    , EString <$> lTemplateString
    , EArray <$> brackets (expr `sepEndBy` comma)
    , EStruct <$> braces (((,) <$> ident <*> (colon *> expr)) `sepEndBy` comma)
    , EFunction <$> (kwFunction *> function)
    , uncurry ENew <$> (kwNew *> funcall)
    , try (uncurry EFuncall <$> funcall)
    , EVariable <$> variable
    ]

expr :: Parser Expr
expr = makeExprParser eTerm opTable <* spaces <?> "expression"

-- * Statements

varDecl :: Parser VarDecl
varDecl = do
    name <- ident_ <* space
    mType <- optional $ inPragma type_
    mInit <- optional $ symbol "=" *> expr
    return $ VarDecl name mInit mType

sDeclare :: Parser Stmt
sDeclare = do
    kwVar
    vars <- varDecl `sepBy1` comma
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

sReturn :: Parser Stmt
sReturn = do
    kwReturn
    expr <- optional expr
    return $ maybe SReturnVoid SReturn expr

sMacro :: Parser Stmt
sMacro = do
    keyword "#macro"
    p1 <- ident_
    c <- optional $ single ':'
    (mConfig, name) <- case c of
        Nothing -> spaces >> return (Nothing, p1)
        Just _ -> do
            p2 <- ident
            return (Just p1, p2)
    SMacro mConfig name <$> expr

-- | A single statement, optionally ended with a semicolon.
stmt :: Parser Stmt
stmt = (choice
    [ SBlock      <$> block
    , sMacro
    , SBreak <$ kwBreak, SContinue <$ kwContinue, SExit <$ kwExit
    , SFunction   <$> (kwFunction *> ident) <*> function
    , SDeclare    <$> (kwVar *> varDecl `sepBy1` comma)
    , SGlobalvar  <$> (kwGlobalvar *> varDecl)
    , SStatic     <$> (kwStatic *> varDecl)
    , sEnum
    , SWith       <$> (kwWith *> parens expr) <*> stmt
    , SRepeat     <$> (kwRepeat *> expr) <*> stmt
    , SWhile      <$> (kwWhile  *> expr) <*> stmt
    , SDoUntil    <$> (kwDo *> stmt) <*> (kwUntil *> expr)
    , sFor
    , SIf         <$> (kwIf *> expr) <*> stmt <*> optional (kwElse *> stmt)
    , sReturn
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
