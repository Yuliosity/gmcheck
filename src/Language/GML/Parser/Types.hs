{-|
Module      : Language.GML.Parser.Types
Description : GML builtin types parser

A parser for signatures of built-in function and variables. See the self-descriptive format in `data/%filename%.ty`.
-}

module Language.GML.Parser.Types where

import Prelude hiding (Enum)

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as M

import Text.Megaparsec

import Language.GML.Types
import Language.GML.Parser.Common

nametype :: Parser (Name, Type)
nametype = do
    tyName <- ident
    -- TODO: flatten
    case scalarTypes M.!? tyName of
        Just res -> return $ (tyName, res)
        Nothing -> case vectorTypes M.!? tyName of
            Just res -> do
                (subname, subtype) <- between (symbol "<") (symbol ">") nametype
                return (tyName ++ "<" ++ subname ++ ">", res subtype)
            Nothing -> case M.lookup tyName scalarTypes of
                Just res -> return (tyName, res)
                Nothing -> fail $ "unknown type: " ++ tyName
    where
        scalarTypes = M.fromList $
            -- Base types
            [ ("void",    TVoid)
            , ("real",    TReal)
            , ("string",  TString)
            , ("ptr",     TPtr)
            -- Derived types
            , ("any",     TAny)
            , ("int",     TInt)
            , ("bool",    TReal)
            , ("char",    TChar)
            , ("alpha",   TAlpha)
            , ("color",   TColor)
            , ("date",    TDate)
            -- Enums
            , ("keycode", TKeyCode)
            -- Resource descriptors
            , ("instance",TInstance)
            , ("background", TBackground)
            , ("font",    TFont)
            , ("object",  TObject)
            , ("path",    TPath)
            , ("room",    TRoom)
            , ("sound",   TSound)
            , ("sprite",  TSprite)
            ] ++ map (\e -> (e, TEnum e))
            [ "event"
            , "mbutton"
            , "primitive"
            ] --TODO: load that from file -}

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

-- * Parsing variable types

vars :: Parser ([Name], Type)
vars = do
    names <- names <* symbol ":"
    (_, ty) <- nametype
    return (names, ty)

-- | Dictionary for holding variable types.
type VarDict = M.Map Name Type

parseVars :: String -> Text -> Result VarDict
parseVars name src = M.fromList . concatMap unpack <$>
    parseMany vars name src

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

-- | Dictionary for holding function signatures.
type FunDict = M.Map Name Signature

unpack :: ([a], b) -> [(a, b)]
unpack (xs, y) = [(x, y) | x <- xs]

parseFun :: String -> Text -> Result FunDict
parseFun name src = M.fromList . concatMap unpack <$>
    parseMany sigs name src

-- * Parsing enums

enum :: Parser Enum
enum = do
    keyword "enum"
    name <- ident
    labels <- braces $ ident `sepBy1` comma
    --TODO: parse values
    return $ Enum name $ zip labels [0..]

type EnumDict = M.Map Name Enum

parseEnum :: String -> Text -> Result EnumDict
parseEnum name src = M.fromList . map (\e@(Enum name _) -> (name, e)) <$> 
    parseMany enum name src
