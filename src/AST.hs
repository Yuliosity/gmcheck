module AST where

import Data.Text

data Statement
    = SEmpty
    | SExpression Expression
    | SVar VarName Expression -- ^ Declaring a local variable
    | SAssign Variable Expression -- ^ Assigning an existing variable
    | SWith VarName Block
    | SFor Statement Expression Expression Block
    | SBreak -- ^ Break from a loop
    | SIf Expression Block Block
    | SReturn Expression
    | SExit -- ^ Exit from a script/event
    deriving (Eq, Show)

type Block = [Statement]

{-| Unary operators. -}
data UnOp = UPreInc | UPostInc | UPreDec | UPostDec | UNot
    deriving (Eq, Show)

{-| Binary operators. -}
data BinOp = BAdd | BSub | BMul | BDiv | BLess | BEq | BGreater
    deriving (Eq, Show)

{-| Expressions which can be evaluated to a value. -}
data Expression
    = EUnary UnOp Expression
    | EBinary Expression BinOp Expression
    | EFuncall FunName [Expression] -- ^ Function/script call with arguments
    | EVariable Variable
    | ELiteral Literal
    deriving (Eq, Show)

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = VVar VarName -- ^ Local, instance or global variable
    | VField Variable VarName -- ^ Field/instance variable
    | VArray Variable Expression -- ^ One-dimensional array, indexed by a number
    | VArray2 Variable (Expression, Expression) -- ^ Two-dimensional array, indexed by two numbers
    deriving (Eq, Show)

type FunName = String

type VarName = String

data Literal = LNumeric Double | LString String
    deriving (Eq, Show)
