{-# LANGUAGE TypeFamilies #-}

module Language.GML.Parser.Common
    ( Parser, Error, Result
    , Name
    , spaces, comma, colon, semicolon
    , parens, braces, brackets
    , lexeme, symbol, keyword, operator, ident
    , manyAll, parseFile
    ) where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.GML.Types (Name)

type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void
type Result a = Either Error a

{-| Spaces and comments skipper. -}
spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

keyword :: Text -> Parser Text
keyword kw = (lexeme . try) (string kw <* notFollowedBy alphaNumChar)

opSymbol :: Parser Char
opSymbol = satisfy (`elem` ("+-*/=<>!|&^" :: String)) --TODO: optimize

operator :: Text -> Parser Text
operator op = (lexeme . try) (string op <* notFollowedBy opSymbol)

parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

comma, colon, semicolon :: Parser ()
comma     = symbol "," $> ()
colon     = symbol ":" $> ()
semicolon = symbol ";" $> ()

{-| Identifier -}
ident :: Parser Name
ident = lexeme $ (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

manyAll :: Parser a -> Parser [a]
manyAll p = spaces *> many p <* eof

parseFile :: Parser [a] -> FilePath -> IO [a]
parseFile parser path = do
    src <- T.readFile path
    case parse parser path src of
        Left err -> putStrLn (errorBundlePretty err) >> return []
        Right err -> return err
