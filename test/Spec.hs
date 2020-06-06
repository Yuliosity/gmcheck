{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Test.Hspec

import AST
import Parser

lit42 = ELit (LNumeric 42)
litString = ELit (LString "string")

foo = VVar "foo"
bar = VVar "bar"
baz = VVar "baz"

vars = describe "variables" $ do
    it "can parse fields" $ do
        parse (variable <* eof) "test" "foo" `shouldBe` Right foo
        parse (variable <* eof) "test" "bar.foo" `shouldBe` Right (VField "bar" foo)
    it "can parse arrays" $ do
        parse (variable <* eof) "test" "foo[42.0]" `shouldBe` Right (VArray foo lit42)
        parse (variable <* eof) "test" "baz.bar[foo]" `shouldBe` Right (VField "baz" $ VArray bar $ EVar foo)
    it "can parse nested fields" $ do
        parse (variable <* eof) "test" "baz[bar[foo]]" `shouldBe` Right (VArray baz $ EVar $ VArray bar $ EVar foo)

simpleExpr = describe "simple expressions" $ do
    it "can parse a single literal" $ do
        parse (expr <* eof) "test" "42.0" `shouldBe` Right lit42
        parse (expr <* eof) "test" "\"string\"" `shouldBe` Right litString
    it "can parse binary operators" $ do
        parse (expr <* eof) "test" "42.0+foo" `shouldBe` Right (EBinary BAdd lit42 $ EVar foo)

main :: IO ()
main = hspec $ do
    vars
    simpleExpr
