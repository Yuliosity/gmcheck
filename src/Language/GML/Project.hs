{-|
Module      : Project
Description : GM Project

Facilities for representing the whole Game Maker project and loading its codebase.
-}

{-# LANGUAGE LambdaCase #-}

module Language.GML.Project
    ( Script (..)
    , Object (..)
    , Project (..)
    , loadProject
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import qualified Data.Map as M
import qualified Data.Text.IO as T (readFile)
import System.Directory
import System.FilePath
import System.IO

import Language.GML.Parser.AST
import Language.GML.Types (Resource (..))
import Language.GML.Events

type RSource = Result Source --FIXME: report errors, not store them in the project

{-| Executable script. -}
data Script = Script
    { sName :: String
    , sSource :: RSource
    }
    deriving Show

{-| Object with callable events. -}
data Object = Object
    { {- oName :: OName
    , -} oEvents :: M.Map Event RSource
    }
    deriving Show

data Project = Project
    { pResources :: M.Map String Resource
    , pScripts   :: M.Map String RSource
    , pObjects   :: M.Map String Object
    }
    deriving Show

resDir :: Resource -> FilePath
resDir = \case
    RSprite     -> "sprites"
    RSound      -> "sounds"
    RBackground -> "backgrounds"
    RRoom       -> "rooms"

logTrace :: String -> IO ()
logTrace = hPutStrLn stderr

loadResources :: FilePath -> Resource -> IO (M.Map String Resource)
loadResources path ty = do
    let rPath = path </> resDir ty
    rNames <- listDirectory rPath
    logTrace $ "Loading resources from " ++ rPath
    return $ M.fromList [(res, ty) | res <- rNames]
    <|>
    return M.empty

{-| Loads the project from a directory. -}
loadProject :: FilePath -> IO Project
loadProject path = do
    -- Load resources
    resources <- mapM (loadResources path)
        [ RSprite
        , RSound
        , RBackground
        , RRoom
        ]
    -- Load scripts
    let sDir = path </> "scripts"
    sNames <- listDirectory sDir
    scripts <- forM sNames $ \name -> do
        let sPath = sDir </> name </> name <.> "gml"
        logTrace $ "Loading a script from " ++ sPath
        src <- T.readFile sPath
        return (name, parseSource name src)
    -- Load objects
    let oDir = path </> "objects"
    oNames <- listDirectory oDir
    objects <- forM oNames $ \name -> do
        eNames <- filter ((== ".gml") . takeExtension) <$> (listDirectory $ oDir </> name)
        events <- forM eNames $ \eName -> do
            let ePath = oDir </> name </> eName
            logTrace $ "Loading an event from " ++ ePath
            src <- T.readFile ePath
            return (read $ dropExtension eName, parseSource eName src)
        return (name, Object (M.fromList events))
    return $ Project (M.unions resources) (M.fromList scripts) (M.fromList objects)
