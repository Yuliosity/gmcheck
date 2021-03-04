{-# LANGUAGE TypeFamilies #-}

module Language.GML.Parser.Common
    ( Parser, Error, Result
    , parseFile
    ) where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type Error = ParseErrorBundle Text Void
type Result a = Either Error a

parseFile :: Parser [a] -> FilePath -> IO [a]
parseFile parser path = do
    src <- T.readFile path
    case parse parser path src of
        Left err -> putStrLn (errorBundlePretty err) >> return []
        Right err -> return err
