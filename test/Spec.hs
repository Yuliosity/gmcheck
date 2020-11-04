{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.GML.AST
import Language.GML.Types
import Language.GML.Parser.AST

lit42 = ELiteral (LNumeric 42)
litPi = ELiteral (LNumeric 3.14)
litString = ELiteral (LString "string")
sin_pi = EFuncall "sin" [litPi]
write_string = EFuncall "write" [litString]
foo = EVariable "foo"
foo_lt_42 = EBinary (BComp Less) foo lit42
bar = EVariable "bar"

parse' p = parse (p <* eof) "test"

vars = describe "variables parser" $ do
    it "can parse fields" $ do
        parse' variable "foo" `shouldParse` "foo"
        parse' variable "bar.foo" `shouldParse` (VField "bar" "foo")
    it "can parse arrays" $ do
        parse' variable "foo[42]" `shouldParse` (VArray "foo" lit42)
        parse' variable "foo[42, 42]" `shouldParse` (VArray2 "foo" (lit42, lit42))
        parse' variable "baz.bar[foo]" `shouldParse` (VField "baz" $ VArray "bar" foo)
    it "can parse nested fields" $ do
        parse' variable "baz[bar[foo]]" `shouldParse` (VArray "baz" $ EVariable $ VArray "bar" foo)
    it "can parse accessors" $ do
        parse' variable "foo[| 42]" `shouldParse` (VContainer SList "foo" lit42)
        parse' variable "foo[# 42, 42]" `shouldParse` (VContainer2 SGrid "foo" (lit42, lit42))

simpleExpr = describe "expressions parser" $ do
    it "can parse a single literal" $ do
        parse' expr "42" `shouldParse` lit42
        parse' expr "\"string\"" `shouldParse` litString
    it "can parse a variable as an expression" $ do
        parse' expr "foo" `shouldParse` foo
        parse' expr "foo[42]" `shouldParse` EVariable (VArray "foo" lit42)
    it "can parse binary operators" $ do
        parse' expr "42+foo" `shouldParse` (EBinary (BNum Add) lit42  foo)
    it "can parse function calls" $ do
        parse' expr "rand()" `shouldParse` EFuncall "rand" []
        parse' expr "sin(3.14)" `shouldParse` sin_pi
        parse' expr "cat(foo, bar)" `shouldParse` EFuncall "cat" [foo, bar]

simpleStmt = describe "statements parser" $ do
    it "can parse variable declarations" $ do
        parse' stmt "var foo" `shouldParse` SDeclare "foo" Nothing
        parse' stmt "var foo=42" `shouldParse` SDeclare "foo" (Just lit42)
    it "can parse variable assignments" $ do
        parse' stmt "foo=42" `shouldParse` SAssign "foo" AAssign lit42
        parse' stmt "foo+=\"string\"" `shouldParse` SAssign "foo" (AModify Add) litString
    it "can parse function calls" $ do
        parse' stmt "write(\"string\")" `shouldParse` SExpression write_string
        parse' stmt "var foo = sin(3.14)" `shouldParse` SDeclare "foo" (Just sin_pi)
    it "can parse conditionals" $ do
        parse' stmt "if foo==42 exit" `shouldParse` SIf (EBinary (BComp Eq) foo lit42) [SExit] []
        parse' stmt "if (foo < 42) exit" `shouldParse` SIf foo_lt_42 [SExit] []
        parse' stmt "if foo bar=42 else exit" `shouldParse` SIf foo [SAssign "bar" AAssign lit42] [SExit]
        parse' stmt "if (foo < 42) {foo += 42 exit}" `shouldParse`
            SIf foo_lt_42 [SAssign "foo" (AModify Add) lit42, SExit] []
    it "can parse loops" $ do
        parse' stmt "while(foo) write(\"string\")" `shouldParse` SWhile foo [SExpression write_string]
        parse' stmt "while(foo) {foo -= 42; write(\"string\")}" `shouldParse`
            SWhile foo [SAssign "foo" (AModify Sub) lit42, SExpression write_string]
    it "can parse multi-lines" $ do
        parse' block "{var foo\nfoo = 42}" `shouldParse` [SDeclare "foo" Nothing, SAssign "foo" AAssign lit42]

complexStmt = describe "complex script parser" $ do
    it "can parse nested loops" $ do
        parse' stmt "while(foo < 42) {while(bar) {write(\"string\");}}" `shouldParse`
            SWhile foo_lt_42 [SWhile bar [SExpression write_string]]


main :: IO ()
main = hspec $ do
    vars
    simpleExpr
    simpleStmt