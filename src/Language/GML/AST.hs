{-|
Module      : Language.GML.AST
Description : GML AST

Everything representing the Game Maker Language source tree.
-}

{-# LANGUAGE PatternSynonyms #-}

module Language.GML.AST where

import Data.String

import Language.GML.Types

-- * GML values

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = VVar   Name          -- ^ Local, self or global variable
    | VField Variable Name -- ^ Field/instance variable (possibly chained)
    | VContainer  Container  Variable Expr         -- ^ Data structure accessor. Arrays are a special case.
    | VContainer2 Container2 Variable (Expr, Expr) -- ^ 2D data structure accessor
    deriving (Eq, Show)

{-| One-dimensional array, indexed by a number. -}
pattern VArray  v e = VContainer  SArray  v e

{-| Two-dimensional array, indexed by two numbers. Legacy in GMS 2.3. -}
pattern VArray2 v e = VContainer2 SArray2 v e

-- * Operators

{-| Arithetical and logical operations, used in both modification assignment and binary operations. -}
data NumOp
    = Add | Sub | Mul | Div
    | Mod | IntDiv
    | Shr | Shl | BitAnd | BitOr | BitXor
    deriving (Eq, Show)

{-| Boolean operations. -}
data BoolOp
    = And | Or | Xor
    deriving (Eq, Show)

{-| Comparison operators. -}
data CompOp
    = Eq | NotEq | Less | Greater | LessEq | GreaterEq
    deriving (Eq, Show)

{-| Unary operators, in order of precedence. -}
data UnOp
    = UBitNeg | UNeg | UNot
    | UPreInc  | UPreDec
    | UPostInc | UPostDec
    deriving (Eq, Show)

{-| Any binary operator. -}
data BinOp
    = BNum  NumOp
    | BComp CompOp
    | BBool BoolOp
    deriving (Eq, Show)

-- * Expressions

type FieldName = String

type FunName = String

{-| Expression which can be evaluated to a value. -}
data Expr
    -- Values
    = EVariable Variable
    | ENumber   Double          -- ^ Numeric literal
    | EString   String          -- ^ String literal
    | EArray    [Expr]          -- ^ Array literal
    | EFunction [Name] Block    -- ^ Inline function
    | EStruct   [(FieldName, Expr)] -- ^ Struct
    -- Operators
    | EUnary    UnOp  Expr      -- ^ Unary expression
    | EBinary   BinOp Expr Expr -- ^ Binary expression
    | ETernary  Expr  Expr Expr -- ^ Ternary conditional [cond ? t : f]
    | EFuncall  Name [Expr]     -- ^ Function/script call with arguments
    deriving (Eq, Show)

-- Helper instances for writing expressions in code

instance IsString Variable where
    fromString = VVar --TODO: parse

instance IsString Expr where
    fromString = EVariable . fromString

instance Num Expr where
    fromInteger = ENumber . fromInteger
    (+) = eBinary Add
    (-) = eBinary Sub
    (*) = eBinary Mul
    negate = EUnary UNeg
    abs x = EFuncall "abs" [x]
    signum x = EFuncall "sign" [x]

instance Fractional Expr where
    fromRational = ENumber . fromRational
    (/) = eBinary Div

class Binary a where
    toBin :: a -> BinOp

instance Binary NumOp where
    toBin = BNum

instance Binary CompOp where
    toBin = BComp

instance Binary BoolOp where
    toBin = BBool

eBinary :: Binary a => a -> Expr -> Expr -> Expr
eBinary = EBinary . toBin

-- * Statements

{-| Statement (instruction). -}
data Stmt
    = SExpression Expr -- ^ Calling an expression (typically a function/script with side effects)
    -- Ddeclarations and modification
    | SDeclare [(Name, Maybe Expr)]  -- ^ Declaring local variable(s)
    | SAssign Variable Expr          -- ^ Assigning a new variable, possibly declaring it in-place
    | SModify NumOp Variable Expr    -- ^ Modifying an existing variable
    | SFunction Name [Name] Block    -- ^ Declaring a function with arguments and a body
    -- Control flow structures
    | SBlock   Block           -- ^ Nested sequence of statements
    | SWith    Expr Stmt       -- ^ Switching the execution context into an another instance
    | SRepeat  Expr Stmt       -- ^ Repeating some instructions several times
    | SWhile   Expr Stmt       -- ^ Loop with a pre-condition
    | SDoUntil Stmt Expr       -- ^ Loop with a post-condition
    | SFor     Stmt Expr Stmt Stmt    -- ^ For loop. TODO: limit the first header stmt to assign or declare, and the second one to assign
    | SIf      Expr Stmt (Maybe Stmt) -- ^ Conditional. If the `else` branch is missing, the second statement is [Nothing].
    | SSwitch  Expr [([Expr], Block)] -- ^ Switch-case. For the default branch, the case list is empty.
    -- Control flow redirection
    | SBreak       -- ^ Break from a loop or switch-case
    | SContinue    -- ^ Continue to the next loop iteration
    | SExit        -- ^ Exit from a script/event without a result
    | SReturn Expr -- ^ Return the result from a script
    deriving (Eq, Show)

{-| A block is a sequence of statements, typically in braces. -}
type Block = [Stmt]

{-| Any GML source is a list of statements. -}
type Program = Block
