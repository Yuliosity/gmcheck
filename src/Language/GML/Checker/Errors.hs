{-# LANGUAGE LambdaCase, OverloadedStrings, StrictData #-}

module Language.GML.Checker.Errors where

import qualified Data.Map.Strict as M

import Language.GML.AST
import Language.GML.Events (Event) 
import Language.GML.Types

data Error
    -- | Changing the variable type
    = WChangeType Variable Type Type
    -- | Different types of array elements
    | WHeteroArray Variable Type Type
    -- | Different types of the ternary operator
    | WTernaryDiff Type Type
    -- | Referencing an unknown variable
    | EUndefinedVar Variable
    -- | Calling an unknown function or script
    | EUndefinedFunction FunName
    -- | Function doesn't return anything
    | ENoResult Variable
    -- | Assigning to a constant
    | EAssignConst Variable
    -- | Argument of `with` is not an instance
    | EWithInstance
    -- | Wrong expression type
    | EWrongExprType String Type Type
    -- | Wrong variable type
    | EWrongVarType Variable Type Type
    -- | Wrong operand of an unary operator
    | EBadUnary UnOp Type
    -- | Wrong operand(s) of a binary operator
    | EBadBinary BinOp Type Type
    -- | Wrong operand(s) of an assignment operator
    -- TODO: unify?
    | EBadModify NumOp Type Type
    -- | Non-numeric array indexing
    | EArrayIndex Variable Type
    -- | Wrong argument count
    | EWrongArgNum FunName Int Int
    -- | Wrong type of function argument
    | EWrongArgument FunName Name Type Type
    -- | Data structure is not destroyed
    | EDataStructureLeak Variable
    deriving Show

type Log = [Error]

{-| Source script. -}
data Source = SScript !Name | SObject !Name !Event
    deriving (Eq, Ord)

{-| Errors report. -}
newtype Report = Report (M.Map Source [Error])

instance Semigroup Report where
    (Report m1) <> (Report m2) = Report (M.unionWith (++) m1 m2)

instance Monoid Report where
    mempty = Report M.empty

singleError :: Source -> Error -> Report
singleError src err = Report $ M.singleton src [err]

addError :: Source -> Error -> Report -> Report
addError src err (Report map) = Report $ M.insertWith (++) src [err] map

{-

WARN
N arguments in a script call, but it uses only K<N
Using a deprecated function/variable
Unspecified arguments evaluation order
Instance variable is not used outside of the scope
Script argument N is missing
Call event_inherited by the object with no parent

ERROR
Uninitialized variable access
Cannot add number and string
Cannot use operator with numbers only
Array access by non-number
Argument type XX, must be YY
N arguments in a script call, but it uses K>N
Call unimplemented event
Call unimplemented script
Not all branches return a value
Assign to a function which doesn't return anything

-}