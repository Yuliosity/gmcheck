{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

import Language.GML.Project

data Options = Options
    { output :: FilePath
    , target :: FilePath
    }

optParser :: Parser Options
optParser = Options
    <$> strOption
        (  long "output"
        <> short 'o'
        <> help "Output report" )
    <*> argument str
        (  help "Path to the GMS project"
        <> metavar "TARGET" )

options :: ParserInfo Options
options = info (helper <*> optParser)
      ( fullDesc
     <> progDesc "Run the checker"
     <> header "gmcheck - a static analysis tool for Game Maker Studio 2 projects" )

main :: IO ()
main = do
    Options { output, target } <- execParser options
    project <- loadProject target
    writeFile output $ show project
