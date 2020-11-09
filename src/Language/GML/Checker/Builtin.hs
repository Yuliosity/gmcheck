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

import qualified Data.Text.IO as T (readFile)
import Text.Megaparsec (errorBundlePretty) 

import Control.Monad (forM)
import Data.Map.Strict ((!?))
import Data.Foldable (asum)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Language.GML.Parser.Types
import Language.GML.Types (Name, Type)

load parser file = do
    src <- T.readFile file
    case parser file src of
        Left err -> error $ errorBundlePretty err
        Right res -> return res

{-| Bundle of type annotations of built-in functions and variables. -}
data Builtin = Builtin
    { bFunctions     :: !FunDict
    , bGlobalConst   :: !VarDict
    , bGlobalVar     :: !VarDict
    , bInstanceConst :: !VarDict
    , bInstanceVar   :: !VarDict
    , bEnums         :: !EnumDict
    }

{-| Looks up for a built-in variable or a constant. -}
lookupBuiltin :: Name -> Builtin -> Maybe (Type, Bool, Bool)
lookupBuiltin name (Builtin {bGlobalConst, bGlobalVar, bInstanceConst, bInstanceVar}) = asum
    [ (, True,  True)  <$> bGlobalConst   !? name
    , (, True,  False) <$> bGlobalVar     !? name
    , (, False, True)  <$> bInstanceConst !? name
    , (, False, False) <$> bInstanceVar   !? name
    ]

{-| Loads a built-in bundle from a directory. TODO: report missing files. -}
loadBuiltin :: FilePath -> IO Builtin
loadBuiltin dir = do
    en <- load parseEnum $ dir </> "enums.gmli"
    fs <- load parseFun $ dir </> "functions.gmli"
    let loadVars = load parseVars
    [gc, gv, ic, iv] <- forM
        ["global_const.gmli", "global_var.gmli", "instance_const.gmli", "instance_var.gmli"] $
        \file -> loadVars (dir </> file)
    return $ Builtin fs gc gv ic iv en

{-| Hardcoded built-in bundle. For testing purposes. -}
testBuiltin :: Builtin
testBuiltin = unsafePerformIO $ loadBuiltin "data"
