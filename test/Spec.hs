{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser

lit42 = ELit (LNumeric 42)
litString = ELit (LString "string")

foo = VVar "foo"
bar = VVar "bar"
baz = VVar "baz"

parse' p = parse (p <* eof)

vars = describe "variables" $ do
    it "can parse fields" $ do
        parse' variable "test" "foo" `shouldParse` foo
        parse' variable "test" "bar.foo" `shouldParse` (VField "bar" foo)
    it "can parse arrays" $ do
        parse' variable "test" "foo[42.0]" `shouldParse` (VArray foo lit42)
        parse' variable "test" "baz.bar[foo]" `shouldParse` (VField "baz" $ VArray bar $ EVar foo)
    it "can parse nested fields" $ do
        parse' variable "test" "baz[bar[foo]]" `shouldParse` (VArray baz $ EVar $ VArray bar $ EVar foo)

simpleExpr = describe "simple expressions" $ do
    it "can parse a single literal" $ do
        parse expr "test" "42.0" `shouldParse` lit42
        parse expr "test" "\"string\"" `shouldParse` litString
    it "can parse binary operators" $ do
        parse expr "test" "42.0+foo" `shouldParse` (EBinary BAdd lit42 $ EVar foo)

main :: IO ()
main = hspec $ do
    vars
    simpleExpr
