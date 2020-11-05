{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

import Language.GML.Project
import Language.GML.Checker.Builtin (loadBuiltin)

data Options = Options
    { oOutput  :: !FilePath
    , oBuiltin :: !FilePath
    , oProject :: !FilePath
    }

optParser :: Parser Options
optParser = Options
    <$> strOption
        (  long "output"
        <> short 'o'
        <> help "Output report" )
    <*> strOption
        (  long "builtin"
        <> short 'b'
        <> value "data"
        <> help "Path to the builtin signatures" )
    <*> argument str
        (  help "Path to the GMS project directory"
        <> metavar "PROJECT" )

options :: ParserInfo Options
options = info (helper <*> optParser)
      ( fullDesc
     <> progDesc "Run the checker"
     <> header "gmcheck - a static analysis tool for Game Maker Studio 2 projects" )

main :: IO ()
main = do
    Options { oOutput, oBuiltin, oProject } <- execParser options
    project <- loadProject oProject
    builtin <- loadBuiltin oBuiltin
    writeFile oOutput $ show project
