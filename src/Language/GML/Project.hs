{-|
Module      : Language.GML.Project
Description : GM Project

Datatypes representing the whole Game Maker project and functions for loading its codebase.
-}

module Language.GML.Project
    ( Script (..)
    , Object (..)
    , Project (..)
    , loadProject
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import qualified Data.Map as M
import Data.Text (Text, pack)
import System.Directory
import System.FilePath
import System.IO

import Language.GML.Parser.Common (parseFile)
import Language.GML.Parser.AST
import Language.GML.Types (Name, Type (TNewtype))
import Language.GML.Events

{-| Executable script. -}
data Script = Script
    { sName :: Name
    , sSource :: Program
    }
    deriving Show

{-| Object with callable events. -}
data Object = Object
    { {- oName :: OName
    , -} oEvents :: M.Map Event Program
    }
    deriving Show

data Project = Project
    { pResources :: M.Map Name Type
    --, pGuids     :: M.Map Guid Name
    , pScripts   :: M.Map Name Program
    , pObjects   :: M.Map Name Object
    }
    deriving Show

logTrace :: String -> IO ()
logTrace = hPutStrLn stderr

loadResources :: FilePath -> String -> IO (M.Map Name Type)
loadResources path ty = do
    let rPath = path </> (ty ++ "s")
    rNames <- listDirectory rPath
    logTrace $ "Loading resources from " ++ rPath
    return $ M.fromList [(pack res, TNewtype $ pack ty) | res <- rNames]
    <|>
    return M.empty

loadProgram :: String -> FilePath -> IO Program
loadProgram what path = do
    logTrace $ "Loading " ++ what ++ " from " ++ path
    parseFile program path

{-| Loads the project from a directory. -}
loadProject :: FilePath -> IO Project
loadProject path = do
    --TODO: load 2.3 projects
    -- Load resources
    resources <- mapM (loadResources path)
        [ "font", "sound", "sprite", "room" ]
    -- Load scripts
    let sDir = path </> "scripts"
    sNames <- listDirectory sDir
    scripts <- forM sNames $ \name -> do
        let name' = case name of
                '@':xs -> xs --Strip the compatibility script prefix
                xs     -> xs
        pr <- loadProgram "script" $ sDir </> name </> name' <.> "gml"
        return (pack name', pr)
    -- Load objects
    let oDir = path </> "objects"
    oNames <- listDirectory oDir
    objects <- forM oNames $ \name -> do
        eNames <- filter ((== ".gml") . takeExtension) <$> listDirectory (oDir </> name)
        events <- forM eNames $ \eName -> do
            pr <- loadProgram "event" $ oDir </> name </> eName
            return (read $ dropExtension eName, pr)
        return (pack name, Object (M.fromList events))
    let resObjects = M.fromList $ zip (map pack oNames) $ repeat (TNewtype "object")
    return $ Project (M.unions $ resObjects : resources) (M.fromList scripts) (M.fromList objects)
