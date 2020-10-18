{-# LANGUAGE LambdaCase #-}

module Language.GML.Checker.Errors where

import qualified Data.Map.Strict as M

import Language.GML.AST
import Language.GML.Events (Event) 
import Language.GML.Types (Type)

data Error
    -- | Changing the variable type
    = WChangeType Variable Type Type
    -- | Different types of array elements
    | WHeteroArray Variable Type Type
    -- | Different types of the ternary operator
    | WTernaryDiff Type Type
    -- | Function doesn't return anything
    | ENoResult Variable
    -- | Wrong expression type
    | EWrongExprType String Type Type
    -- | Wrong variable type
    | EWrongVarType Variable Type Type
    -- | Wrong operand of an unary operator
    | EBadUnary UnOp Type
    -- | Wrong operand(s) of a binary operator
    | EBadBinary BinOp Type Type
    -- | Non-numeric array indexing
    | EArrayIndex Variable Type
    -- | No such function or script
    | EUnknownFunction FunName
    -- | Wrong type of function argument
    | EWrongArgument FunName Name Type Type
    -- | Data structure is not destroyed
    | EDataStructureLeak Variable
    deriving Show

type Log = [Error]

{-| Errors report. -}
data Report = Report
    { rScripts :: M.Map Name Log
    , rObjects :: M.Map Name [(Event, Log)]
    }

pretty :: Error -> String
pretty = \case
    WChangeType var from to -> "Type of " ++ show var ++ " might be changed from " ++ show from ++ " to " ++ show to
    EWrongExprType descr need ty -> "Type of " ++ descr ++ "should be " ++ show need ++ ", but seems to be " ++ show ty 
    EWrongVarType var need ty -> "Type of " ++ show var ++ "should be " ++ show need ++ ", but seems to be " ++ show ty 
    EBadUnary op ty -> "Unary operation " ++ show op ++ " cannot be applied to the type " ++ show ty
    EArrayIndex var ty -> "Trying to index the array " ++ show var ++ " with not a number, but " ++ show ty
    EWrongArgument fun name need ty -> "Argument " ++ name ++ " of function " ++ fun ++ " should be " ++ show need ++ ", but seems to be " ++ show ty
    err -> "Raw error: " ++ show err


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