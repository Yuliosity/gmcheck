module Project where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Text.IO as T (readFile)
import System.Directory
import System.FilePath
import System.IO

import AST
import Parser

data KeyState = Pressed | Hold | Released
    deriving Show
    
data KeyCode = Up | Down | Char Char
    deriving Show   

instance Enum KeyCode where
    fromEnum = undefined
    toEnum = undefined

data Event
    = Create
    | Step
    | Draw
    | Key KeyState KeyCode
    deriving Show

instance Enum Event where
    fromEnum = undefined
    toEnum = undefined

data Script = Script
    { scName :: String
    , scSource :: Source
    }
    deriving Show

data Object = Object
    { obName :: String
    , obEvents :: M.Map String Source
    }
    deriving Show

data Project = Project
    { pScripts :: M.Map String Source
    , pObjects :: M.Map String Object
    }
    deriving Show

loadProject :: FilePath -> IO Project
loadProject path = do
    let sDir = path </> "scripts"
    sNames <- listDirectory sDir
    scripts <- forM sNames $ \name -> do
        let sPath = sDir </> name </> name <.> "gml"
        hPutStrLn stderr $ "Loading a script from " ++ sPath
        src <- T.readFile sPath
        return (name, parseSource name src)
    return $ Project (M.fromList scripts) M.empty
