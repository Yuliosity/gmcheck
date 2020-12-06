{-|
Module      : Language.GML.Checker.Builtin
Description : Built-in GML functions

Types of built-in instance variables and signatures of library functions.
-}

{-# LANGUAGE TupleSections #-}

module Language.GML.Checker.Builtin
    ( Builtin (..)
    , VarDict, FunDict
    , loadBuiltin, testBuiltin
    ) where

import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Language.GML.Parser.Common (parseFile)
import Language.GML.Parser.Types
import Language.GML.Types

-- | Dictionary for holding variable types.
type VarDict = M.Map Name VarType

-- | Dictionary for holding function signatures.
type FunDict = M.Map Name Signature

{-| Bundle of type annotations of built-in functions and variables. -}
data Builtin = Builtin
    { bFunctions     :: !FunDict
    , bGlobalVar     :: !VarDict
    , bInstanceVar   :: !VarDict
    }

{-| Loads a built-in bundle from a directory. TODO: report missing files. -}
loadBuiltin :: FilePath -> IO Builtin
loadBuiltin dir = do
    en <- parseFile enums $ dir </> "enums.gmli"
    let enumValues = M.fromList [(name, (TNewtype ty, True)) | Enum ty opts <- en, (name, _) <- opts]
    fs <- M.fromList <$> parseFile functions (dir </> "functions.gmli")
    let loadVars file = M.fromList <$> parseFile variables file
    [globVars, instVars] <- mapM (\file -> loadVars (dir </> file))
        ["global.gmli", "instance.gmli"]
    return $ Builtin fs (M.union enumValues globVars) instVars

{-| Hardcoded built-in bundle. For testing purposes. -}
testBuiltin :: Builtin
testBuiltin = unsafePerformIO $ loadBuiltin "data"
