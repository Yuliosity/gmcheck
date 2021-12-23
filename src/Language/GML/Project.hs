{-|
Module      : Language.GML.Project
Description : GM Project

Datatypes representing the whole Game Maker project and functions for loading its codebase.
-}

{-# LANGUAGE TypeApplications #-}

module Language.GML.Project
    ( Script (..)
    , Object (..)
    , Room (..)
    , Project (..)
    , loadProject
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.List (find)
import qualified Data.Map as M
import Data.Text (Text, pack)
import qualified Data.YAML as Yaml

import System.Directory
import System.FilePath
import System.IO

import Language.GML.Parser.Common (parseFile)
import Language.GML.Parser.AST
import Language.GML.Types (Name, Type (TNewtype))
import Language.GML.Events

{-| Executable script. -}
data Script = Script
    { sName   :: Name
    , sSource :: Program
    }
    deriving Show

{-| Object with callable events. -}
data Object = Object
    { oName   :: Name
    , oEvents :: M.Map Event Program
    }
    deriving Show

{-| Rooms with game objects. -}
data Room = Room
    { rName          :: Name
    , rCreationOrder :: [Text]
    , rCreationCode  :: Maybe Program
    }
    deriving Show

instance Yaml.FromYAML Room where
  parseYAML = Yaml.withMap "Room" $ \m -> Room
    <$> pure "FIXME"
    <*> m Yaml..: "instanceCreationOrder"
    <*> pure Nothing

data Project = Project
    { pResources :: M.Map Name Type
    --, pGuids     :: M.Map Guid Name
    , pScripts   :: M.Map Name Program
    , pObjects   :: M.Map Name Object
    , pRooms     :: M.Map Name Room
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

loadScripts :: FilePath -> IO (M.Map Name Program)
loadScripts path = do
    let dir = path </> "scripts"
    names <- listDirectory dir
    scripts <- forM names $ \name -> do
        let name' = case name of
                '@':xs -> xs --Strip the compatibility script prefix
                xs     -> xs
        pr <- loadProgram "script" $ dir </> name </> name' <.> "gml"
        return (pack name', pr)
    pure (M.fromList scripts)

loadObjects :: FilePath -> IO ([FilePath], M.Map Name Object)
loadObjects path = do
    let dir = path </> "objects"
    names <- listDirectory dir
    objects <- forM names $ \name -> do
        eNames <- filter (isExtensionOf "gml") <$> listDirectory (dir </> name)
        events <- forM eNames $ \eName -> do
            pr <- loadProgram "event" $ dir </> name </> eName
            return (read $ dropExtension eName, pr)
        return Object
            { oName = pack name
            , oEvents = M.fromList events
            }
    pure (names, M.fromList $ map (\o -> (oName o, o)) objects)

loadRooms :: FilePath -> IO ([FilePath], M.Map Name Room)
loadRooms path = do
    let dir = path </> "rooms"
    names <- listDirectory dir
    rooms <- forM names $ \name -> do
        files <- listDirectory (dir </> name)
        let configName = head $ filter ("yy" `isExtensionOf`) files
            creationCodeName = find ("gml" `isExtensionOf`) files
        rCreationCode <- traverse
            (\codename -> loadProgram "creation code" (dir </> name </> codename))
            creationCodeName
        config <- fromRight (Room (pack name) [] Nothing)
                . Yaml.decode1Strict @Room
                <$> BS.readFile (dir </> name </> configName)
        return config {rCreationCode}
    pure (names, M.fromList $ map (\o -> (rName o, o)) rooms)

createResoursesMap :: [FilePath] -> Type -> M.Map Name Type
createResoursesMap names resType = M.fromList $ zip (map pack names) $ repeat resType

{-| Loads the project from a directory. -}
loadProject :: FilePath -> IO Project
loadProject path = do
    --TODO: load 2.3 projects
    -- Load resources
    resources <- mapM (loadResources path)
        [ "font", "sound", "sprite" ]
    -- Load scripts
    pScripts <- loadScripts path
    (oFilenames, pObjects) <- loadObjects path
    (rFilenames, pRooms)   <- loadRooms path
    let resObjects = createResoursesMap oFilenames (TNewtype "object")
        resRooms   = createResoursesMap rFilenames (TNewtype "room")
    return $ Project
        { pResources = M.unions $ resRooms : resObjects : resources
        , pScripts
        , pObjects
        , pRooms
        }
