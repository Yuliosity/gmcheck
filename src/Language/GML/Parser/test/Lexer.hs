module Lexer where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.GML.Parser.Lexer

parse' p = parse (p <* eof) "test"

numbers = describe "numbers parser" $ do
    it "can parse hexadecimals" $ do
        parse' lNumber "$1234" `shouldParse` 0x1234
        parse' lNumber "0x1234" `shouldParse` 0x1234
    it "can parse floats" $ do
        parse' lNumber "3.14" `shouldParse` 3.14
        parse' lNumber "3.14e10" `shouldParse` 3.14e10
        parse' lNumber ".14" `shouldParse` 0.14

strings = describe "strings parser" $ do
    it "can parse strings" $ do
        parse' lString "\"string\"" `shouldParse` "string"
        parse' lString "\"1\\n2\\n3\"" `shouldParse` "1\n2\n3"
    it "can parse multiline strings" $ do
        parse' lString "@\"1\n2\n3\"" `shouldParse` "1\n2\n3"

test = hspec $ do
    strings
