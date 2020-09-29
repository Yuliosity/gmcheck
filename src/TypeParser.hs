{-# LANGUAGE OverloadedStrings #-}

module TypeParser where

import Data.Maybe (fromMaybe)
import Data.Text hiding (empty, map)
import Data.Void (Void)
import qualified Data.Map.Strict as M

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST (Name)
import Types

type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void

{-| Spaces and comments skipper. -}
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

nametype :: Parser (Name, Type)
nametype = do
    tyName <- ident
    case M.lookup tyName types of
        Just res -> return (tyName, res)
        Nothing -> fail $ "unknown type: " ++ tyName
    where
        types = M.fromList
            [ ("void",    TVoid)
            , ("real",    TReal)
            , ("int",     TReal)
            , ("alpha",   TReal) --between 0 and 1
            , ("bool",    TReal)
            , ("string",  TString)
            , ("color",   TColor)
            , ("id",      TReal)
            , ("sprite",  TId RSprite)
            , ("sound",   TId RSound)
            , ("object",  TId RObject)
            , ("room",    TId RRoom)
            , ("mbutton", TReal) --FIXME: enum
            , ("vkey",    TReal) --FIXME: enum
            , ("event",   TReal) --FIXME: enum
            ]

ident :: Parser Name
ident = lexeme $ (:) <$> letterChar <*> many alphaNumChar

parens, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

arg :: Parser Argument
arg = do
    name <- optional $ brackets ident
    (tyName, ty) <- nametype
    return $ (fromMaybe tyName name, ty)

sig :: Parser (Name, Signature)
sig = do
    name <- ident
    args <- sepBy1 arg (symbol ",")
    symbol "->"
    (_, ret) <- nametype
    return (name, args :-> ret)

-- | Dictionary for holding variable types.
type VarDict = M.Map Name Type

parseVars :: String -> Text -> Either Error VarDict
parseVars = undefined

-- | Dictionary for holding function signatures.
type FunDict = M.Map Name Signature

parseFun :: String -> Text -> Either Error FunDict
parseFun = undefined
