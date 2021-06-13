{-|
Module      : Language.GML.Parser.Types
Description : GML builtin types parser

A parser for signatures of built-in function and variables. See the self-descriptive format in `data/%filename%.ty`.
-}

module Language.GML.Parser.Types
    ( VarType
    , type_, signature_
    , variables, signatures, enums
    ) where

import Prelude hiding (Enum)

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char (char)

import Language.GML.Types
import Language.GML.Parser.Common
import Language.GML.Parser.Lexer

nametype :: Parser (Name, Type)
nametype =
    try function <|> do
    tyName <- ident
    -- TODO: flatten
    case tyName of
        -- TODO: better rule for type variables
        _ | T.length tyName == 1 -> return (tyName, TTypeVar tyName)
        _ | Just res <- scalarTypes M.!? tyName -> return (tyName, res)
        _ | Just res <- vectorTypes M.!? tyName -> do
                (subname, subtype) <- between (symbol "<") (symbol ">") nametype
                return (tyName <> "<" <> subname <> ">", res subtype)
        _ -> return (tyName, TNewtype tyName)
    where
        scalarTypes = M.fromList
            -- Base types
            [ ("void",    TVoid)
            , ("real",    TReal)
            , ("string",  TString)
            , ("ptr",     TPtr)
            , ("matrix",  TMatrix)
            -- , ("function", TFunction [] TAny)
            , ("struct",  TStruct []) --TODO: which fields?
            -- Derived types
            , ("any",     TAny)
            , ("int",     TInt)
            , ("bool",    TBool)
            , ("char",    TChar)
            , ("alpha",   TAlpha)
            , ("instance",TInstance)
            , ("layer",   TUnknown [TNewtype "layer_id", TString])
            , ("exception", TException)
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

function = do
    --TODO: parse single argument without parens
    args <- parens (arg `sepBy` comma)
    symbol "->"
    ret <- type_
    --TODO: show function type
    return ("function", TFunction args ret)

type_ = snd <$> nametype

names :: Parser [Name]
names = ident `sepBy1` comma

unpack :: ([a], b) -> [(a, b)]
unpack (xs, y) = [(x, y) | x <- xs]

-- * Parsing variable types

type VarType = (Type, Bool)

vars :: Parser ([Name], VarType)
vars = do
    isConst <- option False $ True <$ keyword "const"
    names <- names <* colon
    ty <- type_
    return (names, (ty, isConst))

variables :: Parser [(Name, VarType)]
variables = concatMap unpack <$> manyAll vars

-- * Parsing builtin function signatures

arg :: Parser Argument
arg = do
    name <- optional (try (ident <* colon))
    (tyName, ty) <- nametype
    return (fromMaybe tyName name, ty)

-- Difference between this and [function] is that it can handle optional and variadic arguments
signature_ :: Parser Signature
signature_ = do
    (args, moreArgs) <- parens (do
        args <- arg `sepBy` comma
        moreArgs <- (VarArgs <$> (symbol "*" *> arg))
                <|> (OptArgs <$> option [] (symbol "?" *> arg `sepBy1` comma))
        return (args, moreArgs))
        <|> do
            arg <- arg
            return ([arg], OptArgs [])
    symbol "->"
    ret <- type_
    return $ Signature args moreArgs ret

signatures :: Parser [(Name, Signature)]
signatures = concatMap unpack <$> manyAll ((,) <$> names <* colon <*> signature_)

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
