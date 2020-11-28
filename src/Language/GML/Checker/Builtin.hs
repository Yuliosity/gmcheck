{-|
Module      : Language.GML.Checker.Builtin
Description : Built-in GML functions

Types of built-in instance variables and signatures of library functions.
-}

{-# LANGUAGE TupleSections #-}

module Language.GML.Checker.Builtin
    ( Builtin (..)
    , VarDict, FunDict
    , loadBuiltin
    , lookupBuiltin, testBuiltin
    ) where

import Control.Monad (forM)
import qualified Data.Map.Strict as M
import Data.Foldable (asum)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Language.GML.Parser.Common (parseFile)
import Language.GML.Parser.Types
import Language.GML.Types

-- | Dictionary for holding variable types.
type VarDict = M.Map Name Type

-- | Dictionary for holding function signatures.
type FunDict = M.Map Name Signature

{-| Bundle of type annotations of built-in functions and variables. -}
data Builtin = Builtin
    { bFunctions     :: !FunDict
    , bGlobalConst   :: !VarDict
    , bGlobalVar     :: !VarDict
    , bInstanceConst :: !VarDict
    , bInstanceVar   :: !VarDict
    }

{-| Looks up for a built-in variable or a constant. -}
lookupBuiltin :: Name -> Builtin -> Maybe (Type, Bool, Bool)
lookupBuiltin name Builtin {bGlobalConst, bGlobalVar, bInstanceConst, bInstanceVar} = asum
    [ (, True,  True)  <$> bGlobalConst   M.!? name
    , (, True,  False) <$> bGlobalVar     M.!? name
    , (, False, True)  <$> bInstanceConst M.!? name
    , (, False, False) <$> bInstanceVar   M.!? name
    ]

{-| Loads a built-in bundle from a directory. TODO: report missing files. -}
loadBuiltin :: FilePath -> IO Builtin
loadBuiltin dir = do
    en <- parseFile enums $ dir </> "enums.gmli"
    let enc = M.fromList [(name, TNewtype ty) | Enum ty opts <- en, (name, _) <- opts]
    fs <- M.fromList <$> parseFile functions (dir </> "functions.gmli")
    let loadVars file = M.fromList <$> parseFile variables file
    [gc, gv, ic, iv] <- forM
        ["global_const.gmli", "global_var.gmli", "instance_const.gmli", "instance_var.gmli"] $
        \file -> loadVars (dir </> file)
    return $ Builtin fs (M.union gc enc) gv ic iv

{-| Hardcoded built-in bundle. For testing purposes. -}
testBuiltin :: Builtin
testBuiltin = unsafePerformIO $ loadBuiltin "data"
