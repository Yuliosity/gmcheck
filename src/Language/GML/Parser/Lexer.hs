{-# LANGUAGE TypeFamilies #-}

module Language.GML.Parser.Lexer where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Text (Text, pack)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.GML.Location
import Language.GML.Parser.Common
import Language.GML.Types (Name)

-- | Spaces and comments skipper.
spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

manyAll :: Parser a -> Parser [a]
manyAll p = spaces *> many p <* eof

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser ()
symbol str = L.symbol spaces str $> ()

-- * Punctuation
comma, colon, semicolon, parenL, parenR, braceL, braceR :: Parser ()
comma = symbol ","
colon = symbol ":"
semicolon = symbol ";"
parenL = symbol "("
parenR = symbol ")"
braceL = symbol "{"
braceR = symbol "}"

parens, braces, brackets :: Parser a -> Parser a
parens = between parenL parenR
braces = between braceL braceR
brackets = between (symbol "[") (symbol "]")

-- * Keywords
keyword :: Text -> Parser ()
keyword kw = (lexeme . try) (string kw <* notFollowedBy alphaNumChar) $> ()

reserved =
    [ "begin"
    , "break"
    , "case"
    , "catch"
    , "constructor"
    , "continue"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "end"
    , "enum"
    , "exit"
    , "finally"
    , "for"
    , "false"
    , "function"
    , "globalvar"
    , "if"
    , "infinity"
    , "NaN"
    , "new"
    , "pi"
    , "pointer_null"
    , "pointer_invalid"
    , "repeat"
    , "return"
    , "static"
    , "switch"
    , "throw"
    , "true"
    , "try"
    , "undefined"
    , "until"
    , "var"
    , "while"
    , "with"
    ]

[ kwBegin
    , kwBreak
    , kwCase
    , kwCatch
    , kwConstructor
    , kwContinue
    , kwDefault
    , kwDelete
    , kwDo
    , kwElse
    , kwEnd
    , kwEnum
    , kwExit
    , kwFinally
    , kwFor
    , kwFalse
    , kwFunction
    , kwGlobalvar
    , kwIf
    , kwInfinity
    , kwNaN
    , kwNew
    , kwPi
    , kwPointerNull
    , kwPointerInvalid
    , kwRepeat
    , kwReturn
    , kwStatic
    , kwSwitch
    , kwThrow
    , kwTrue
    , kwTry
    , kwUndefined
    , kwUntil
    , kwVar
    , kwWhile
    , kwWith
    ] = map keyword reserved

-- * Operators
opSymbol :: Parser Char
opSymbol = satisfy (`elem` ("+-*/=<>!|&^" :: String)) -- TODO: optimize

operator :: Text -> Parser Text
operator op = (lexeme . try) (string op <* notFollowedBy opSymbol)

-- | Identifier
ident :: Parser Name
ident = try $ do
    i <- lexeme $ pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    guard $ i `notElem` reserved
    return i

-- * Literals

-- | Number literal.
lNumber :: Parser Double
lNumber =
    ( try (lexeme (L.signed empty L.float))
        <|> fromIntegral
        <$> lexeme (L.signed empty L.decimal)
    )
        <?> "number"

-- | String literal.
lString :: Parser Text
lString =
    -- TODO: escaped characters
    pack <$> (char '\"' *> manyTill L.charLiteral (char '\"') <* spaces)
        <?> "string"

located :: Parser a -> Parser (Located a)
located p = Located . toPos <$> getSourcePos <*> p
  where
    toPos (SourcePos _file line col) = Pos (unPos line) (unPos col)
