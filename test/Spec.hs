{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.GML.AST
import Language.GML.Parser.AST

lit42 = ELit (LNumeric 42)
litString = ELit (LString "string")
foo = VVar "foo"
bar = VVar "bar"

parse' p = parse (p <* eof) "test"

vars = describe "variables parser" $ do
    it "can parse fields" $ do
        parse' variable "foo" `shouldParse` foo
        parse' variable "bar.foo" `shouldParse` (VField "bar" foo)
    it "can parse arrays" $ do
        parse' variable "foo[42]" `shouldParse` (VArray "foo" lit42)
        parse' variable "baz.bar[foo]" `shouldParse` (VField "baz" $ VArray "bar" $ EVar foo)
    it "can parse nested fields" $ do
        parse' variable "baz[bar[foo]]" `shouldParse` (VArray "baz" $ EVar $ VArray "bar" $ EVar foo)

simpleExpr = describe "expressions parser" $ do
    it "can parse a single literal" $ do
        parse' expr "42" `shouldParse` lit42
        parse' expr "\"string\"" `shouldParse` litString
    it "can parse a variable as an expression" $ do
        parse' expr "foo" `shouldParse` EVar foo
        parse' expr "foo[42]" `shouldParse` EVar (VArray "foo" lit42)
    it "can parse binary operators" $ do
        parse' expr "42+foo" `shouldParse` (EBinary (BNum Add) lit42 $ EVar foo)
    it "can parse function calls" $ do
        parse' expr "rand()" `shouldParse` EFuncall "rand" []
        parse' expr "sin(42)" `shouldParse` EFuncall "sin" [lit42]
        parse' expr "cat(foo, bar)" `shouldParse` EFuncall "cat" [EVar foo, EVar bar]

simpleStmt = describe "statements parser" $ do
    it "can parse variable declarations" $ do
        parse' stmt "var foo" `shouldParse` SDeclare "foo" Nothing
        parse' stmt "var foo=42" `shouldParse` SDeclare "foo" (Just lit42)
    it "can parse variable assignments" $ do
        parse' stmt "foo=42" `shouldParse` SAssign foo AAssign lit42
        parse' stmt "foo+=\"string\"" `shouldParse` SAssign foo (AModify Add) litString
    it "can parse conditionals" $ do
        parse' stmt "if foo==42 exit" `shouldParse` SIf (EBinary (BComp Eq) (EVar foo) lit42) [SExit] []
        parse' stmt "if foo bar=42 else exit" `shouldParse` SIf (EVar foo) [SAssign bar AAssign lit42] [SExit]
    let foo_lt_42 = EBinary (BComp Less) (EVar foo) lit42
    it "can parse parens" $ do
        parse' stmt "if (foo < 42) exit" `shouldParse` SIf foo_lt_42 [SExit] []
    it "can parse blocks" $ do
        parse' stmt "if (foo < 42) {foo += 42 exit}" `shouldParse`
            SIf foo_lt_42 [SAssign foo (AModify Add) lit42, SExit] []
    it "can parse multi-lines" $ do
        parse' block "{var foo\nfoo = 42}" `shouldParse` [SDeclare "foo" Nothing, SAssign foo AAssign lit42]

main :: IO ()
main = hspec $ do
    vars
    simpleExpr
    simpleStmt