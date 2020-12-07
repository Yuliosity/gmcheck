{-|
Module      : Language.GML.Parser.Types
Description : GML builtin types parser

A parser for signatures of built-in function and variables. See the self-descriptive format in `data/%filename%.ty`.
-}

module Language.GML.Parser.Types
    ( VarType (..), variables, functions, enums )
    where

import Prelude hiding (Enum)

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Text.Megaparsec

import Language.GML.Types
import Language.GML.Parser.Common

nametype :: Parser (Name, Type)
nametype = do
    tyName <- ident
    -- TODO: flatten
    case scalarTypes M.!? tyName of
        Just res -> return (tyName, res)
        Nothing -> case vectorTypes M.!? tyName of
            Just res -> do
                (subname, subtype) <- between (symbol "<") (symbol ">") nametype
                return (tyName ++ "<" ++ subname ++ ">", res subtype)
            Nothing -> return (tyName, TNewtype tyName)
    where
        scalarTypes = M.fromList
            -- Base types
            [ ("void",    TVoid)
            , ("real",    TReal)
            , ("string",  TString)
            , ("ptr",     TPtr)
            , ("matrix",  TMatrix)
            -- Derived types
            , ("any",     TAny)
            , ("int",     TInt)
            , ("bool",    TBool)
            , ("char",    TChar)
            , ("alpha",   TAlpha)
            , ("instance",TInstance)
            , ("layer",   TUnknown [TNewtype "layer_id", TString])
            ]

        vectorTypes = M.fromList
            [ ("array",   TArray)
            , ("array2",  TArray2)
            , ("grid",    TGrid)
            , ("list",    TList)
            , ("map",     TMap)
            , ("pqueue",  TPriorityQueue)
            , ("queue",   TQueue)
            , ("stack",   TStack)
            ]

names :: Parser [Name]
names = sepBy1 ident (symbol ",")

unpack :: ([a], b) -> [(a, b)]
unpack (xs, y) = [(x, y) | x <- xs]

-- * Parsing variable types

type VarType = (Type, Bool)

vars :: Parser ([Name], VarType)
vars = do
    isConst <- option False $ True <$ keyword "const"
    names <- names <* symbol ":"
    (_, ty) <- nametype
    return (names, (ty, isConst))

variables :: Parser [(Name, VarType)]
variables = concatMap unpack <$> manyAll vars

-- * Parsing function signatures

arg :: Parser Argument
arg = do
    name <- optional $ brackets ident
    (tyName, ty) <- nametype
    return (fromMaybe tyName name, ty)

sigs :: Parser ([Name], Signature)
sigs = do
    names <- names <* symbol ":"
    rawArgs <- arg `sepBy` symbol ","
    let args = case rawArgs of
                    [(_, TVoid)] -> []
                    xs -> xs
    moreArgs <- (VarArgs <$> (symbol "*" *> arg))
            <|> (OptArgs <$> option [] (symbol "?" *> arg `sepBy1` symbol ","))
    symbol "->"
    (_, ret) <- nametype
    return (names, Signature args moreArgs ret)

functions :: Parser [(Name, Signature)]
functions = concatMap unpack <$> manyAll sigs

-- * Parsing enums

enum :: Parser Enum
enum = do
    keyword "enum"
    name <- ident
    labels <- braces $ ident `sepBy1` comma
    --TODO: parse values
    return $ Enum name $ zip labels [0..]

enums :: Parser [Enum]
enums = manyAll enum
