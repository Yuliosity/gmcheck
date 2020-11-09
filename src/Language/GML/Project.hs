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
import qualified Data.Text.IO as T (readFile)
import System.Directory
import System.FilePath
import System.IO

import Language.GML.Parser.AST
import Language.GML.Types (Resource (..))
import Language.GML.Events
import Text.Megaparsec (errorBundlePretty)

{-| Executable script. -}
data Script = Script
    { sName :: String
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
    { pResources :: M.Map String Resource
    , pScripts   :: M.Map String Program
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

loadProgram :: String -> FilePath -> IO Program
loadProgram what path = do
    logTrace $ "Loading " ++ what ++ " from " ++ path
    src <- T.readFile path
    case parseProgram path src of
        Left err -> putStrLn (errorBundlePretty err) >> return []
        Right err -> return err

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
        pr <- loadProgram "script" $ sDir </> name </> name <.> "gml"
        return (name, pr)
    -- Load objects
    let oDir = path </> "objects"
    oNames <- listDirectory oDir
    objects <- forM oNames $ \name -> do
        eNames <- filter ((== ".gml") . takeExtension) <$> (listDirectory $ oDir </> name)
        events <- forM eNames $ \eName -> do
            pr <- loadProgram "event" $ oDir </> name </> eName
            return (read $ dropExtension eName, pr)
        return (name, Object (M.fromList events))
    return $ Project (M.unions resources) (M.fromList scripts) (M.fromList objects)
