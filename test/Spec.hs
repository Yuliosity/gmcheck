{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Test.Hspec

import AST
import Parser

lit42 = ELiteral (LNumeric 42)
litString = ELiteral (LString "string")

simpleExpr = describe "simple expressions" $ do
    it "can parse a single literal" $ do
        parse (expression <* eof) "test" "42.0" `shouldBe` Right lit42
        parse (expression <* eof) "test" "\"string\"" `shouldBe` Right litString
    it "can parse binary operators" $ do
        parse (expression <* eof) "test" "42.0+42.0" `shouldBe` Right (EBinary lit42 BAdd lit42)

main :: IO ()
main = hspec $ do
    simpleExpr
