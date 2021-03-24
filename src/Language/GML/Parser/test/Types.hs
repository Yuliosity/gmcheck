module Types where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.GML.Types
import Language.GML.Parser.Types hiding (functions)

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

functions = describe "function types" $ do
    let shouldParseAs = shouldParse . parse' signature_
    it "can parse simple function signatures" $ do
        "void -> bool" `shouldParseAs` ([] :-> TBool)
        "int, bool -> string" `shouldParseAs` ([("int", TInt), ("bool", TBool)] :-> TString)
        "string -> array<string>" `shouldParseAs` ([("string", TString)] :-> TArray TString)
    it "can parse named arguments" $ do
        "[x] real, [y] real -> real" `shouldParseAs` ([("x", TReal), ("y", TReal)] :-> TReal)
    it "can parse optional arguments" $ do
        "int ? [opt] int -> bool" `shouldParseAs` Signature [("int", TInt)] (OptArgs [("opt", TInt)]) TBool
    it "can parse variadic arguments" $ do
        "array<int> * [val] int -> void" `shouldParseAs` Signature [("array<int>", TArray TInt)] (VarArgs ("val", TInt)) TVoid

test = hspec $ do
    types
    functions
