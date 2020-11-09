{-# LANGUAGE LambdaCase, OverloadedStrings, StrictData #-}

module Language.GML.Checker.Errors where

import qualified Data.Map.Strict as M
import Text.Blaze
import Text.Blaze.Html5 as H

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

{-| Errors report. -}
data Report = Report
    { rScripts :: M.Map Name Log
    , rObjects :: M.Map Name [(Event, Log)]
    }

instance ToMarkup Variable where
    toMarkup = \case
        VVar name -> toMarkup name
        VField name var -> do toMarkup name; "."; toMarkup var
        VContainer  _con name _expr      -> do toMarkup name; "[..]" --TODO: show expr
        VContainer2 _con name (_e1, _e2) -> do toMarkup name; "[..,..]" --TODO: show expr

instance ToMarkup UnOp where
    toMarkup = \case
        UBitNeg  -> "~"
        UNeg     -> "-"
        UNot     -> "!"
        UPreDec  -> "--"
        UPreInc  -> "++"
        UPostDec -> "--"
        UPostInc -> "++"

instance ToMarkup BinOp where
    toMarkup = \case
        BNum Add -> "+"
        BNum Sub -> "-"
        BNum Mul -> "*"
        BNum Div -> "/"
        BNum Mod -> "mod"
        BNum IntDiv -> "div"
        BNum BitAnd -> "&"
        BNum BitOr  -> "|"
        BNum BitXor -> "^"
        BNum Shr    -> ">>"
        BNum Shl    -> "<<"
        BBool And -> "&&"
        BBool Or  -> "||"
        BBool Xor -> "^^"
        BComp Eq        -> "=="
        BComp NotEq     -> "!="
        BComp Less      -> "<"
        BComp LessEq    -> "<="
        BComp Greater   -> ">"
        BComp GreaterEq -> ">="

instance ToMarkup NumOp where
    toMarkup = \case
        Add -> "+="
        Sub -> "-="
        Mul -> "*="
        Div -> "/="
        BitAnd -> "&="
        BitOr  -> "|="
        BitXor -> "^="
        op  -> toMarkup $ show op --impossible

instance ToMarkup Container where
    toMarkup = \case
        SArray -> "array"
        SStack -> "stack"
        SList  -> "list"
        SMap   -> "map"
        SQueue -> "queue"
        SPriorityQueue -> "pqueue"

instance ToMarkup Container2 where
    toMarkup = \case
        SArray2 -> "array2"
        SGrid   -> "grid"

instance ToMarkup Resource where
    toMarkup = \case
        RBackground -> "background"
        RFont       -> "font"
        RObject     -> "object"
        RPath       -> "path"
        RRoom       -> "room"
        RSound      -> "sound"
        RSprite     -> "sprite"

instance ToMarkup Type where
    toMarkup = \case
        TUnknown opt -> do "{"; markupMany opt; "}"
        TVoid   -> "void"
        TReal   -> "real"
        TString -> "string"
        TColor  -> "color"
        TId res -> toMarkup res
        TEnum name -> toMarkup name
        TContainer  con ty -> do toMarkup con; "<"; toMarkup ty; ">"
        TContainer2 con ty -> do toMarkup con; "<"; toMarkup ty; ">"
        where
            markupMany [] = mempty
            markupMany [x] = toMarkup x
            markupMany (x:xs) = do toMarkup x; "|"; markupMany xs

instance ToMarkup Error where
    toMarkup = \case
        WChangeType var from to -> do "Type of "; toMarkup var; " might be changed from "; toMarkup from; " to "; toMarkup to
        EUndefinedVar var -> do "Referencing an undefined variable "; toMarkup var
        EUndefinedFunction fun -> do "Calling an undefined variable "; toMarkup fun
        ENoResult fun -> do "Function "; toMarkup fun; " doesn't return anything"
        EWrongExprType descr need ty -> do "Type of "; toMarkup descr; " should be "; toMarkup need; ", but seems to be "; toMarkup ty
        EWrongVarType  var   need ty -> do "Type of "; toMarkup var;   " should be "; toMarkup need; ", but seems to be "; toMarkup ty
        EWrongArgNum fun n1 n2 -> do "Function "; toMarkup fun; " is expected to have "; toMarkup n1; " arguments, but got "; toMarkup n2
        EBadUnary  op ty    -> do "Operator "; toMarkup op; " cannot be applied to "; toMarkup ty
        EBadBinary op t1 t2 -> do "Operator "; toMarkup op; " cannot be applied to "; toMarkup t1; " and "; toMarkup t2
        EBadModify op t1 t2 -> do "Operator "; toMarkup op; " cannot be applied to "; toMarkup t1; " and "; toMarkup t2
        EAssignConst var -> do "Cannot assign to a constant "; toMarkup var
        EArrayIndex var ty -> do "Trying to index the array "; toMarkup var; " with "; toMarkup ty; " instead of an int"
        EWrongArgument fun name need ty -> do "Argument "; toMarkup name; " of function "; toMarkup fun; " should be "; toMarkup need; ", but seems to be "; toMarkup ty
        err -> toMarkup $ show err
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