{-|
Module      : AST
Description : GML AST

Everything representing the Game Maker Language source tree.
-}

module AST where

import Data.Text

{-| Statement (instruction). -}
data Stmt
    = SEmpty
    | SExpression Expr -- ^ Calling an expression (typically a function/script with side effects)
    | SDeclare VarName (Maybe Expr) -- ^ Declaring a local variable
    | SAssign Variable AssignOp Expr -- ^ Assigning or modifying an existing variable
    | SWith VarName Block -- ^ Switchig the execution context to an another instance
    | SIf Expr Block Block -- ^ Conditional. If the `else` branch is missing, the second block is simply empty
    | SRepeat Expr Block -- ^ Repeating some instructions several times
    | SWhile Expr Block -- ^ Loop with a pre-condition
    | SDoUntil Block Expr -- ^ Loop with a post-condition
    | SFor Stmt Expr Expr Block {-TODO: limit to assign/declare -}
    | SSwitch Expr [([Expr], Block)]
    | SBreak -- ^ Break from a loop or switch-case
    | SContinue 
    | SExit -- ^ Exit from a script/event
    | SReturn Expr -- ^ Return the result from a script
    deriving (Eq, Show)

{-| Any GML source is a list of statements. -}
type Source = [Stmt]

{-| A code sub-block is a bracketed list of statements. -}
type Block = Source

{-| Assigning operations, possibly with arithmetical/boolean modification. -}
data AssignOp
    = AAssign
    | AAdd | ASub | AMul | ADiv
    | AAnd | AOr | AXor
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
    = VVar VarName -- ^ Local, self or global variable
    | VField VarName Variable -- ^ Field/instance variable
    | VArray VarName Expr -- ^ One-dimensional array, indexed by a number
    | VArray2 VarName (Expr, Expr) -- ^ Two-dimensional array, indexed by two numbers
    deriving (Eq, Show)

type FunName = String

type VarName = String

{-| Literal constant in a source. -}
data Literal = LNumeric Double | LString String
    deriving (Eq, Show)
