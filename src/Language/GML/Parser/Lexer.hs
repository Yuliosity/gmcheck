{-# LANGUAGE TypeFamilies #-}

module Language.GML.Parser.Lexer where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as T (readFile)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.GML.Parser.Common
import Language.GML.Types (Name)

{-| Spaces and comments skipper. -}
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
comma     = symbol ","
colon     = symbol ":"
semicolon = symbol ";"
parenL    = symbol "("
parenR    = symbol ")"
braceL    = symbol "{"
braceR    = symbol "}"

parens, braces, brackets :: Parser a -> Parser a
parens = between parenL parenR
braces = between braceL braceR
brackets = between (symbol "[") (symbol "]")

-- * Keywords
keyword :: Text -> Parser ()
keyword kw = (lexeme . try) (string kw <* notFollowedBy alphaNumChar) $> ()

reserved =
    [ "begin", "break", "case", "continue", "default", "do", "else", "end", "enum", "exit", "for"
    , "function", "globalvar", "if", "repeat", "return", "switch", "until", "var", "while", "with"
    ]

[ kwBegin, kwBreak, kwCase, kwContinue, kwDefault, kwDo, kwElse, kwEnd, kwEnum, kwExit, kwFor
    , kwFunction, kwGlobalvar, kwIf, kwRepeat, kwReturn, kwSwitch, kwUntil, kwVar, kwWhile, kwWith
    ] = map keyword reserved

-- * Operators
opSymbol :: Parser Char
opSymbol = satisfy (`elem` ("+-*/=<>!|&^" :: String)) --TODO: optimize

operator :: Text -> Parser Text
operator op = (lexeme . try) (string op <* notFollowedBy opSymbol)

{-| Identifier -}
ident :: Parser Name
ident = try $ do
    i <- lexeme $ (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    guard (pack i `notElem` reserved)
    return i

-- * Literals

-- |Number literal.
lNumber :: Parser Double
lNumber = 
    (try (lexeme (L.signed empty L.float))
    <|> fromIntegral <$> lexeme (L.signed empty L.decimal))
    <?> "number"

-- |String literal.
lString :: Parser String
lString =
    (char '\"' *> manyTill L.charLiteral (char '\"') <* spaces)
    <?> "string"
