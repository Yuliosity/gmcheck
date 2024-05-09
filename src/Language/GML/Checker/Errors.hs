{-# LANGUAGE StrictData #-}

module Language.GML.Checker.Errors (
    module Language.GML.Checker.Errors,
    Source (..),
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Language.GML.AST
import Language.GML.Types

data Error
    = -- | Changing the variable type
      WChangeType Variable Type Type
    | -- | Different types of array elements
      WHeteroArray Variable Type Type
    | -- | Different types of the ternary operator
      WTernaryDiff Type Type
    | -- | Data structure is not destroyed
      WDataStructureLeak Variable
    | -- | Function always throws an exception
      WAlwaysThrow Name
    | -- | Unreachable code after returns or throws
      WUnreachable -- TODO: position
    | -- | Referencing an unknown variable
      EUndefinedVar Variable
    | -- | Calling an unknown function or script
      EUndefinedFunction Name
    | -- | Function doesn't return anything
      ENoResult Variable
    | -- | Assigning to a constant
      EAssignConst Variable
    | -- | Argument of `with` is not an instance
      EWithInstance Type
    | -- | Wrong expression type
      EWrongExprType String Type Type
    | -- | Wrong variable type
      EWrongVarType Variable Type Type
    | -- | Wrong operand of an unary operator
      EBadUnary UnOp Type
    | -- | Wrong operand(s) of a binary operator
      EBadBinary BinOp Type Type
    | -- | Wrong operand(s) of an assignment operator
      -- TODO: unify?
      EBadModify ModifyOp Type Type
    | -- | Wrong index type of a container
      EBadIndex Container Type
    | EBadIndex2 Container2 Type
    | -- | Wrong argument count
      EWrongArgNum Name Ordering Int Int
    | -- | Wrong type of function argument
      EWrongArgument Name Name Type Type
    | -- | Not every branch returns a value
      ENotAlwaysReturn Name
    deriving (Show)

errNum :: Error -> Int
errNum = \case
    WChangeType{} -> 1
    WHeteroArray{} -> 6
    WTernaryDiff{} -> 5
    WDataStructureLeak{} -> 9
    -- Mismatched type
    EWrongArgument{} -> 20
    EWrongVarType{} -> 21
    EWrongExprType{} -> 22
    EWithInstance{} -> 22
    EBadModify{} -> 23
    EBadUnary{} -> 24
    EBadBinary{} -> 24
    EBadIndex{} -> 26
    EBadIndex2{} -> 26
    -- Misc
    EUndefinedVar{} -> 40
    EUndefinedFunction{} -> 41
    ENoResult{} -> 42
    EAssignConst{} -> 43
    _ -> 0

type ErrorSet = S.Set Int

fromList :: [Int] -> ErrorSet
fromList = S.fromList

inSet :: Error -> ErrorSet -> Bool
inSet err = S.member (errNum err)

type LocError = Located Error
type Log = [LocError]

-- | Errors report.
newtype Report = Report (M.Map Source Log)

instance Semigroup Report where
    (Report m1) <> (Report m2) = Report (M.unionWith (++) m1 m2)

instance Monoid Report where
    mempty = Report M.empty

singleError :: Source -> LocError -> Report
singleError src err = Report $ M.singleton src [err]

addError :: Source -> LocError -> Report -> Report
addError src err (Report map) = Report $ M.insertWith (++) src [err] map
