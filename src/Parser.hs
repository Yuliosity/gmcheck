{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Text hiding (empty, map)
import Data.Void (Void)

import AST

type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void

-- $token
-- Basic tokens

{-| Spaces and comments skipper. -}
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy alphaNumChar)

parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

semicolon = symbol ";"

ident = lexeme $ (:) <$> letterChar <*> many alphaNumChar

varName = ident <?> "variable"
funName = ident <?> "function or script"

{-| Number literal.-}
lNumeric :: Parser Literal
lNumeric = LNumeric <$>
    (try (lexeme (L.signed empty L.float))
    <|> fromIntegral <$> lexeme (L.signed empty L.decimal))

{-| String literal.-}
lString :: Parser Literal
lString = LString <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

literal = lNumeric <|> lString

-- $expr
-- Expressions

variable = do
    name <- varName
    choice
        [ VField name <$> (symbol "." *> variable)
        , try $ VArray name <$> brackets expr
        , VArray2 name <$> brackets ((,) <$> expr <*> (symbol "," *> expr))
        , pure $ VVar name
        ]

funcall = EFuncall <$> funName <*> parens (sepBy expr $ symbol ",")

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

eTerm = choice [parens expr, ELit <$> literal, try funcall, EVar <$> variable]

expr :: Parser Expr
expr = makeExprParser eTerm opTable <?> "expression"

-- $stmt
-- Statements

assignOp = choice (map (\(c, s) -> c <$ symbol s) ops) <?> "assignment" where
    ops =
        [ (AAssign, "="), (AAssign, ":=")
        , (AMod Add, "+="), (AMod Sub, "-=")
        , (AMod Mul, "*="), (AMod Div, "/=")
        , (AMod Or, "|="), (AMod And, "&="), (AMod Xor, "^=")
        ]

block = ((symbol "{" <|> keyword "begin") *> manyTill stmt (symbol "}" <|> keyword "end"))
    <|> (:[]) <$> stmt

stmt :: Parser Stmt
stmt = choice
    [ SDeclare <$> (keyword "var" *> varName) <*> optional (assignOp *> expr)
    , SWith <$> (keyword "with" *> varName) <*> block
    , SIf <$> (keyword "if" *> expr) <*> block <*> option [] (keyword "else" *> block)
    , SRepeat <$> (keyword "repeat" *> expr) <*> block
    , SWhile <$> (keyword "while" *> expr) <*> block
    , SDoUntil <$> (keyword "do" *> block) <*> (keyword "until" *> expr)
    , SBreak <$ keyword "break", SContinue <$ keyword "continue", SExit <$ keyword "exit"
    , SReturn <$> (keyword "return" *> expr)
    , SAssign <$> variable <*> assignOp <*> expr
    ] <?> "statement"


type Result = Either Error Source

parseSource :: String -> Text -> Result
parseSource = parse (sc *> many stmt <* eof)
