{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

import Language.GML.Project
import Language.GML.Checker (runChecker)
import Language.GML.Checker.Builtin (loadBuiltin)
import Language.GML.Checker.Errors
import Language.GML.Checker.Render (htmlReport, save)

data Options = Options
    { oOutput  :: !FilePath
    , oBuiltin :: !FilePath
    , oDisErr  :: !String
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
    <*> strOption
        (  long "errors"
        <> short 'e'
        <> value ""
        <> help "Enable or disable individual errors" )
    <*> argument str
        (  help "Path to the GMS project directory"
        <> metavar "PROJECT" )

options :: ParserInfo Options
options = info (helper <*> optParser)
      ( fullDesc
     <> progDesc "Run the checker"
     <> header "gmcheck - a static analysis tool for Game Maker Studio 2 projects" )

parseErrList :: String -> ErrorSet
parseErrList = fromList . map parseErr . words where
    parseErr ('-':xs) = read xs
    --TODO: parse '+'

main :: IO ()
main = do
    Options { oOutput, oBuiltin, oDisErr, oProject } <- execParser options
    project <- loadProject oProject
    builtin <- loadBuiltin oBuiltin
    let disErr = parseErrList oDisErr
    let log = runChecker builtin project disErr
    save oOutput $ htmlReport log
