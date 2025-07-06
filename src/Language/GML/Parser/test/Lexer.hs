module Lexer where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.GML.Parser.Lexer

parse' p = parse (p <* eof) "test"

strings = describe "strings parser" $ do
    it "can parse strings" $ do
        parse' lString "\"string\"" `shouldParse` "string"
        parse' lString "\"1\\n2\\n3\"" `shouldParse` "1\n2\n3"
    it "can parse multiline strings" $ do
        parse' lMultiLineString "@\"1\n2\n3\"" `shouldParse` "1\n2\n3"

test = hspec $ do
    strings
