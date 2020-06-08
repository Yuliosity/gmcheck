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

parse' p = parse (p <* eof) "test"

vars = describe "variables" $ do
    it "can parse fields" $ do
        parse' variable "foo" `shouldParse` foo
        parse' variable "bar.foo" `shouldParse` (VField "bar" foo)
    it "can parse arrays" $ do
        parse' variable "foo[42.0]" `shouldParse` (VArray foo lit42)
        parse' variable "baz.bar[foo]" `shouldParse` (VField "baz" $ VArray bar $ EVar foo)
    it "can parse nested fields" $ do
        parse' variable "baz[bar[foo]]" `shouldParse` (VArray baz $ EVar $ VArray bar $ EVar foo)

simpleExpr = describe "simple expressions" $ do
    it "can parse a single literal" $ do
        parse' expr "42.0" `shouldParse` lit42
        parse' expr "\"string\"" `shouldParse` litString
    it "can parse binary operators" $ do
        parse' expr "42.0+foo" `shouldParse` (EBinary BAdd lit42 $ EVar foo)
{-
    it "can parse function calls" $ do
        parse' expr "rand()" `shouldParse` EFuncall "rand" []
        parse' expr "sin(42.0)" `shouldParse` EFuncall "sin" [lit42]
        parse' expr "cat(foo, bar)" `shouldParse` EFuncall "cat" [EVar foo, EVar bar]
-}

simpleStmt = describe "simple statements" $ do
    it "can parse variable declarations" $ do
        parse' stmt "var foo" `shouldParse` SDeclare "foo" Nothing
        parse' stmt "var foo=42.0" `shouldParse` SDeclare "foo" (Just lit42)
    it "can parse variable assignments" $ do
        parse' stmt "foo=42.0" `shouldParse` SAssign foo AAssign lit42
        parse' stmt "bar+=\"string\"" `shouldParse` SAssign bar AAdd litString

main :: IO ()
main = hspec $ do
    vars
    simpleExpr
    simpleStmt