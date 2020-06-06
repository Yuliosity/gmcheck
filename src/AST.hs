module AST where

import Data.Text

data Stmt
    = SEmpty
    | SExpression Expr
    | SVar VarName Expr -- ^ Declaring a local variable
    | SAssign Variable Expr -- ^ Assigning an existing variable
    | SWith VarName Block
    | SFor Stmt Expr Expr Block {-TODO: limit to assign/declare -}
    | SBreak -- ^ Break from a loop
    | SIf Expr Block Block
    | SReturn Expr
    | SExit -- ^ Exit from a script/event
    deriving (Eq, Show)

type Block = [Stmt]

{-| Unary operators. -}
data UnOp = UPreInc | UPostInc | UPreDec | UPostDec | UNeg | UNot
    deriving (Eq, Show)

{-| Binary operators. -}
data BinOp = BAdd | BSub | BMul | BDiv | BLess | BEq | BGreater
    deriving (Eq, Show)

{-| Expressions which can be evaluated to a value. -}
data Expr
    = EUnary UnOp Expr
    | EBinary BinOp Expr Expr
    | EFuncall FunName [Expr] -- ^ Function/script call with arguments
    | EVar Variable
    | ELit Literal
    deriving (Eq, Show)

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = VVar VarName -- ^ Local, instance or global variable
    | VField VarName VarName -- ^ Field/instance variable
    | VArray Variable Expr -- ^ One-dimensional array, indexed by a number
    | VArray2 Variable (Expr, Expr) -- ^ Two-dimensional array, indexed by two numbers
    deriving (Eq, Show)

type FunName = String

type VarName = String

data Literal = LNumeric Double | LString String
    deriving (Eq, Show)
