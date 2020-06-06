{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Test.Hspec

import AST
import Parser

lit42 = ELit (LNumeric 42)
litString = ELit (LString "string")

simpleExpr = describe "simple expressions" $ do
    it "can parse a single literal" $ do
        parse (expr <* eof) "test" "42.0" `shouldBe` Right lit42
        parse (expr <* eof) "test" "\"string\"" `shouldBe` Right litString
    it "can parse binary operators" $ do
        parse (expr <* eof) "test" "42.0+42.0" `shouldBe` Right (EBinary BAdd lit42 lit42)

main :: IO ()
main = hspec $ do
    simpleExpr
