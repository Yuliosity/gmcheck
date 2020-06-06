module AST where

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

type Block = [Statement]

{-| Unary operators. -}
data UnOp = UPreInc | UPostInc | UPreDec | UPostDec | UNot

{-| Binary operators. -}
data BinOp = BAdd | BSub | BMul | BDiv | BLess | BEq | BGreater

{-| Expressions which can be evaluated to a value. -}
data Expression
    = EUnary UnOp Expression
    | EBinary Expression BinOp Expression
    | EFuncall FunName [Expression] -- ^ Function/script call with arguments
    | EVariable Variable
    | ELiteral Literal

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = LVar VarName -- ^ Local, instance or global variable
    | LField Variable VarName -- ^ Field/instance variable
    | LArray Variable Expression -- ^ One-dimensional array, indexed by a number
    | LArray2 Variable (Expression, Expression) -- ^ Two-dimensional array, indexed by two numbers

type FunName = String

type VarName = String

data Literal = LNumeric Double | LString String
