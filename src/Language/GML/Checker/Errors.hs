{-# LANGUAGE StrictData #-}

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
    | EWithInstance Type
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
    -- | Wrong index type of a container
    | EBadIndex  Container Type
    | EBadIndex2 Container2 Type
    -- | Wrong argument count
    | EWrongArgNum FunName Ordering Int Int
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
