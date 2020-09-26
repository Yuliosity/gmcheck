{-# LANGUAGE LambdaCase #-}

module Project where

import Control.Applicative ((<|>))
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text.IO as T (readFile)
import System.Directory
import System.FilePath
import System.IO

import AST
import Parser
import Types (Resource (..))

data KeyState = Press | Hold | Release
    deriving (Eq, Ord, Show)
    
data KeyCode = KUp | KDown | KLeft | KRight | KChar Char
    deriving (Eq, Ord, Show)   

instance Enum KeyCode where
    toEnum x = case x of
        37 -> KLeft
        39 -> KRight
    fromEnum x = case x of
        KLeft -> 37
        KRight -> 39

data Event
    = Create
    | Step
    | Draw
    | Key KeyState KeyCode
    deriving (Eq, Ord, Show)

parseEvent :: String -> Event
parseEvent str = case name of
    "Create" -> Create
    "Step" -> Step
    "KeyPress" -> Key Press code
    "Keyboard" -> Key Hold code
    "KeyRelease" -> Key Release code
    where
        (name, _:scode) = break (== '_') str
        code = toEnum $ read scode

instance Enum Event where
    fromEnum = undefined
    toEnum = undefined

data Script = Script
    { sName :: String
    , sSource :: Result
    }
    deriving Show

type OName = String

data Object = Object
    { {- oName :: OName
    , -} oEvents :: M.Map Event Result
    }
    deriving Show

data Project = Project
    { pResources :: M.Map String Resource
    , pScripts   :: M.Map String Result
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
            return (parseEvent $ dropExtension eName, parseSource eName src)
        return (name, Object (M.fromList events))
    return $ Project (M.unions resources) (M.fromList scripts) (M.fromList objects)
