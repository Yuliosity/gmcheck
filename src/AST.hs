module AST where

import Data.Text

data Stmt
    = SEmpty
    | SExpression Expr
    | SDeclare VarName (Maybe Expr) -- ^ Declaring a local variable
    | SAssign Variable Expr -- ^ Assigning an existing variable
    | SModify Variable ModifyOp Expr
    | SWith VarName Block
    | SRepeat Expr Block
    | SWhile Expr Block
    | SDoUntil Stmt Block
    | SFor Stmt Expr Expr Block {-TODO: limit to assign/declare -}
    | SSwitch Expr [Block]
    | SBreak -- ^ Break from a loop or switch-case
    | SContinue 
    | SIf Expr Block Block
    | SReturn Expr
    | SExit -- ^ Exit from a script/event
    deriving (Eq, Show)

type Block = [Stmt]

data ModifyOp
    = MAdd | MSub | MMul | MDiv
    | MAnd | MOr | MXor
    deriving (Eq, Show)

{-| Unary operators, in order of precedence. -}
data UnOp
    = UBitNeg | UNeg | UNot
    | UPreInc | UPreDec
    | UPostInc | UPostDec
    deriving (Eq, Show)

{-| Binary operators, in order of precedence. -}
data BinOp
    = BIntDiv | BMod
    | BMul | BDiv
    | BAdd | BSub 
    | BEq | BNotEq | BLess | BGreater | BLessEq | BGreaterEq
    | BAnd | BOr | BXor | BShr | BShl
    | BBitAnd | BBitOr | BBitXor
    deriving (Eq, Show)

{-| Expressions which can be evaluated to a value. -}
data Expr
    = EUnary UnOp Expr
    | EBinary BinOp Expr Expr
    | ETernary Expr Expr Expr -- ^ Ternary conditional [cond ? t : f]
    | EFuncall FunName [Expr] -- ^ Function/script call with arguments
    | EVar Variable
    | ELit Literal
    deriving (Eq, Show)

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = VVar VarName -- ^ Local, instance or global variable
    | VField VarName Variable -- ^ Field/instance variable
    | VArray Variable Expr -- ^ One-dimensional array, indexed by a number
    | VArray2 Variable (Expr, Expr) -- ^ Two-dimensional array, indexed by two numbers
    deriving (Eq, Show)

type FunName = String

type VarName = String

data Literal = LNumeric Double | LString String
    deriving (Eq, Show)
