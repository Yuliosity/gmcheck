{-# LANGUAGE LambdaCase #-}

module Errors where

import AST
import Types

data Error
    -- | Changing the variable type
    = WChangeType Variable Type Type
    -- | Wrong variable type
    | EWrongType Variable Type Type
    -- | Wrong operand of an unary operator
    | EBadUnary UnOp Type
    -- | Wrong operand(s) of a binary operator
    | EBadBinary BinOp Type Type
    -- | Non-numeric array indexing
    | EArrayIndex Variable Type
    deriving Show

pretty :: Error -> String
pretty = \case
    WChangeType var from to -> "Type of " ++ show var ++ " might be changed from " ++ show from ++ " to " ++ show to
    EWrongType var act need -> "Type of " ++ show var ++ " is derived to be " ++ show act ++ ", but should be " ++ show need
    EBadUnary op ty -> "Unary operation " ++ show op ++ " cannot be applied to the type " ++ show ty
    EArrayIndex var ty -> "Trying to index the array " ++ show var ++ " with not a number, but " ++ show ty
    err -> "Raw error: " ++ show err
{-

WARN
Variable type change
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