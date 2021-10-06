{-|
Module      : Language.GML.Project
Description : GM Project

Datatypes representing the whole Game Maker project and functions for loading its codebase.
-}

{-# LANGUAGE DerivingStrategies 
           , GeneralizedNewtypeDeriving 
           , BlockArguments 
           , DeriveAnyClass 
           , DeriveGeneric 
           #-}

{-# LANGUAGE TypeApplications #-}
module Language.GML.Project
    ( Script (..)
    , Object (..)
    , RoomObject (..)
    , RoomConfig (..)
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
import Data.List (find)
import qualified Data.YAML as Yaml
import qualified Data.ByteString as BS
import Data.Either (fromRight)

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


newtype RoomConfig = RoomConfig { instanceCreationOrder::[Text] }
  deriving Show

instance Yaml.FromYAML RoomConfig where
  parseYAML = Yaml.withMap "RoomConfig" \m ->  RoomConfig 
    <$> m Yaml..: "instanceCreationOrder"


data RoomObject = RoomObject RoomConfig (Maybe Program)
  deriving Show

data Project = Project
    { pResources :: M.Map Name Type
    --, pGuids     :: M.Map Guid Name
    , pScripts   :: M.Map Name Program
    , pObjects   :: M.Map Name Object
    , pRooms     :: M.Map Name RoomObject
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
    scripts <- forM names \name -> do
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
    objects <- forM names \name -> do
        eNames <- filter (isExtensionOf "gml") <$> listDirectory (dir </> name)
        events <- forM eNames $ \eName -> do
            pr <- loadProgram "event" $ dir </> name </> eName
            return (read $ dropExtension eName, pr)
        return (pack name, Object (M.fromList events))
    pure (names, M.fromList objects)



loadRooms :: FilePath -> IO ([FilePath], M.Map Name RoomObject)
loadRooms path = do 
    let dir = path </> "rooms"
    names  <- listDirectory dir
    rooms <- forM names \name -> do
        files <- listDirectory (dir </> name)
        let configName = head $ filter (isExtensionOf "yy") files
        let creationCodeName = find (isExtensionOf "gml") files

        creationCode <- traverse 
                  (\codename -> loadProgram "creation code" (dir </> name </> codename)) 
                  creationCodeName
        
        config <- fromRight (RoomConfig []) 
                . Yaml.decode1Strict @RoomConfig 
                <$> BS.readFile (dir </> name </> configName) 

        pure (pack name, RoomObject config creationCode)
    pure (names, M.fromList rooms)



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
    scripts <- loadScripts path
    (oNames, objects) <- loadObjects path
    (rNames, rooms)  <-  loadRooms path
    let resObjects = createResoursesMap oNames (TNewtype "object")
    let resRooms   = createResoursesMap rNames (TNewtype "room")
    return $ Project (M.unions $ resRooms : resObjects : resources) scripts objects rooms