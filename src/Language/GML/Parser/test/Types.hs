module Types where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Debug
import Language.GML.Parser.Lexer

import Language.GML.Types
import Language.GML.Parser.Types hiding (signatures)
import Language.GML.Parser.Lexer (inPragma)

parse' p = parse (p <* eof) "test"

types = describe "types" $ do
    let shouldParseAs = shouldParse . parse' type_
    it "can parse simple types" $ do
        "void" `shouldParseAs` TVoid
        "bool" `shouldParseAs` TBool
    it "can parse vector types" $ do
        "array<int>" `shouldParseAs` TArray TInt
        "list<grid<string>>" `shouldParseAs` TList (TGrid TString)
    it "can parse newtypes" $ do
        "sprite" `shouldParseAs` TNewtype "sprite"
    it "can parse function types" $ do
        "(int) -> int" `shouldParseAs` TFunction [("int", TInt)] TInt
        "() -> bool" `shouldParseAs` TFunction [] TBool
        "(flag: bool, fun: (int) -> int) -> int" `shouldParseAs`
            TFunction [("flag", TBool), ("fun", TFunction [("int", TInt)] TInt)] TInt
    let tyvarT = TTypeVar "T"
    it "can parse type variables" $ do
        "array<T>" `shouldParseAs` TArray tyvarT
        "(a: T, b: T) -> T" `shouldParseAs` TFunction [("a", tyvarT), ("b", tyvarT)] tyvarT

signatures = describe "signatures" $ do
    let shouldParseAs = shouldParse . parse' signature_
    it "can parse simple function signatures" $ do
        "() -> bool" `shouldParseAs` ([] :-> TBool)
        "int -> bool" `shouldParseAs` ([("int", TInt)] :-> TBool)
        "(int, bool) -> string" `shouldParseAs` ([("int", TInt), ("bool", TBool)] :-> TString)
        "string -> array<string>" `shouldParseAs` ([("string", TString)] :-> TArray TString)
    it "can parse named arguments" $ do
        "(string, length:int) -> string" `shouldParseAs` ([("string", TString), ("length", TInt)] :-> TString)
        "(x: real, y: real) -> real" `shouldParseAs` ([("x", TReal), ("y", TReal)] :-> TReal)
    it "can parse optional arguments" $ do
        "(int ? opt: int) -> bool" `shouldParseAs` Signature [("int", TInt)] (OptArgs [("opt", TInt)]) TBool
        "(? real, opt: int) -> bool" `shouldParseAs` Signature [] (OptArgs [("real", TReal), ("opt", TInt)]) TBool
    it "can parse variadic arguments" $ do
        "(array<int> * val: int) -> void" `shouldParseAs` Signature [("array<int>", TArray TInt)] (VarArgs ("val", TInt)) TVoid

variablesTest = describe "variables" $ do
    let shouldParseAs = shouldParse . parse' variables
    it "can parse single var declaration" $ do
        "some_var : real" `shouldParseAs` [("some_var", (TReal, False))]
    it "can parse multiple var declarations" $ do
        "some_var, other_var, another_var : real" `shouldParseAs` [
            ("some_var", (TReal, False)), 
            ("other_var", (TReal, False)), 
            ("another_var", (TReal, False))]
    it "can parse single const declaration" $ do
        "const some_var : real" `shouldParseAs` [("some_var", (TReal, True))]
    it "can parse multiple const declarations" $ do
        "const some_var, other_var, another_var : real" `shouldParseAs` [
            ("some_var", (TReal, True)), 
            ("other_var", (TReal, True)), 
            ("another_var", (TReal, True))]

misc = describe "misc parser" $ do
    it "can parse pragmas with types" $ do
        parse' (inPragma type_) "/* : string */ " `shouldParse` "string"

test = hspec $ do
    types
    signatures
    variablesTest
    misc
