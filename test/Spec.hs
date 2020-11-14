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
foo_42 = VArray "foo" lit42
foo_lt_42 = EBinary (BComp Less) foo lit42
bar = EVariable "bar"

parse' p = parse (p <* eof) "test"

vars = describe "variables parser" $ do
    it "can parse fields" $ do
        parse' variable "foo" `shouldParse` "foo"
        parse' variable "bar.foo" `shouldParse` VField "bar" "foo"
    it "can parse arrays" $ do
        parse' variable "foo[42]" `shouldParse` foo_42
        parse' variable "foo[42, 42]" `shouldParse` ("foo" `VArray2` (lit42, lit42))
        parse' variable "baz.bar[foo]" `shouldParse` ("baz" `VField` "bar" `VArray` foo)
    it "can parse nested indices" $ do
        parse' variable "baz[bar[foo]]" `shouldParse` ("baz" `VArray` EVariable ("bar" `VArray` foo))
    it "can parse accessors" $ do
        parse' variable "foo[|42]" `shouldParse` (VContainer SList "foo" lit42)
        parse' variable "foo[# 42, 42]" `shouldParse` (VContainer2 SGrid "foo" (lit42, lit42))
    it "can parse chained stuff for future" $ do
        parse' variable "baz.bar.foo" `shouldParse` ("baz" `VField` "bar" `VField` "foo")
        parse' variable "foo[42][42][42]" `shouldParse` ("foo" `VArray` lit42 `VArray` lit42 `VArray` lit42)

exprs = describe "expressions parser" $ do
    it "can parse a single literal" $ do
        parse' expr "42" `shouldParse` lit42
        parse' expr "\"string\"" `shouldParse` litString
    it "can parse a variable as an expression" $ do
        parse' expr "foo" `shouldParse` foo
        parse' expr "foo[42]" `shouldParse` EVariable foo_42
    it "can parse binary operators" $ do
        parse' expr "42+foo" `shouldParse` (42 + foo)
    it "can parse prefix and postfix operators" $ do
        parse' expr "foo++" `shouldParse` EUnary UPostInc foo
        parse' expr "foo-- - --foo" `shouldParse` (EUnary UPostDec foo - EUnary UPreDec foo)
    it "can parse function calls" $ do
        parse' expr "rand()" `shouldParse` EFuncall "rand" []
        parse' expr "sin(3.14)" `shouldParse` sin_pi
        parse' expr "cat(foo, bar)" `shouldParse` EFuncall "cat" [foo, bar]
    it "can parse array literals" $ do
        parse' expr "[foo, bar]" `shouldParse` EArray [foo, bar]
        parse' expr "test([foo, 42 + 42])" `shouldParse` EFuncall "test" [EArray [foo, 42 + 42]]

stmts = describe "statements parser" $ do
    it "can parse variable declarations" $ do
        parse' stmt "var foo" `shouldParse` SDeclare [("foo", Nothing)]
        parse' stmt "var foo, bar" `shouldParse` SDeclare [("foo", Nothing), ("bar", Nothing)]
        parse' stmt "var foo=42" `shouldParse` SDeclare [("foo", Just lit42)]
    it "can parse variable assignments" $ do
        parse' stmt "foo=42" `shouldParse` SAssign "foo" lit42
        parse' stmt "foo+=\"string\"" `shouldParse` SModify Add "foo" litString
        parse' stmt "foo[42] -= 42" `shouldParse` SModify Sub foo_42 lit42
    it "can parse variable modifications" $ do
        parse' stmt "foo += 42" `shouldParse` SModify Add "foo" lit42
        parse' stmt "foo++" `shouldParse` SExpression (EUnary UPostInc foo)
    it "can parse function calls" $ do
        parse' stmt "write(\"string\")" `shouldParse` SExpression write_string
        parse' stmt "var foo = sin(3.14)" `shouldParse` SDeclare [("foo", Just sin_pi)]
    it "can parse conditionals" $ do
        parse' stmt "if foo==42 exit" `shouldParse` SIf (eBinary Eq foo lit42) SExit Nothing
        parse' stmt "if (foo < 42) exit" `shouldParse` SIf foo_lt_42 SExit Nothing
        parse' stmt "if foo bar=42 else exit" `shouldParse` SIf foo (SAssign "bar" lit42) (Just SExit)
        parse' stmt "if (foo < 42) {foo += 42 exit}" `shouldParse`
            SIf foo_lt_42 (SBlock [SModify Add "foo" lit42, SExit]) Nothing
    it "can parse loops" $ do
        parse' stmt "while(foo) write(\"string\")" `shouldParse` SWhile foo (SExpression write_string)
        parse' stmt "while(foo) {foo -= 42; write(\"string\")}" `shouldParse`
            SWhile foo (SBlock [SModify Sub "foo" lit42, SExpression write_string])
        parse' stmt "for(foo = 42; foo > 0; foo--) ++bar" `shouldParse`
            SFor (SAssign "foo" lit42) (eBinary Greater foo (ELiteral $ LNumeric 0)) (SExpression $ EUnary UPostDec foo) (SExpression $ EUnary UPreInc bar) 
        parse' stmt "for(var foo=42; foo < bar; foo+=42) write(\"string\")" `shouldParse`
            SFor (SDeclare [("foo", Just lit42)]) (eBinary Less foo bar) (SModify Add "foo" lit42) (SExpression write_string)

programs = describe "complex script parser" $ do
    it "can parse multi-lines" $ do
        parse' program "var foo\nfoo = 42" `shouldParse` [SDeclare [("foo", Nothing)], SAssign "foo" lit42]
    it "can parse nested loops" $ do
        parse' program "while(foo < 42) {while(bar) {write(\"string\");}}" `shouldParse`
            [SWhile foo_lt_42 $ SBlock [SWhile bar $ SBlock [SExpression write_string]]]

main :: IO ()
main = hspec $ do
    vars
    exprs
    stmts
    programs
