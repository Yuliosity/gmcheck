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

{-| Literal constant in a source. -}
data Literal = LNumeric Double | LString String
    deriving (Eq, Show)

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = VVar    Name              -- ^ Local, self or global variable
    | VField  Name Variable     -- ^ Field/instance variable (possibly chained)
    | VContainer  Container  Name Expr -- ^ Data structure accessor. Arrays are a special case.
    | VContainer2 Container2 Name (Expr, Expr) -- ^ 2D data structure accessor
    | VAcc    Name 
    deriving (Eq, Show)

instance IsString Variable where
    fromString = VVar

{-| One-dimensional array, indexed by a number. -}
pattern VArray n e = VContainer SArray n e

{-| Two-dimensional array, indexed by two numbers. Legacy in GMS 2.3. -}
pattern VArray2 n e = VContainer2 SArray2 n e

-- * Operators

{-| Arithetical and logical operations, used in both modification assignment and binary operations. -}
data NumOp
    = Add | Sub | Mul | Div
    | Mod | IntDiv
    | And | Or | Xor
    deriving (Eq, Show)

{-| Comparison operators. -}
data CompOp
    = Eq | NotEq | Less | Greater | LessEq | GreaterEq
    deriving (Eq, Show)

{-| Bitwise operations. -}
data BitOp
    = Shr | Shl | BitAnd | BitOr | BitXor
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
    | BBit  BitOp
    deriving (Eq, Show)

-- * Expressions

type FunName = String

{-| Expression which can be evaluated to a value. -}
data Expr
    = EUnary    UnOp Expr       -- ^ Unary expression
    | EBinary   BinOp Expr Expr -- ^ Binary expression
    | ETernary  Expr Expr Expr  -- ^ Ternary conditional [cond ? t : f]
    | EFuncall  Name [Expr]     -- ^ Function/script call with arguments
    | EVariable Variable
    | ELiteral  Literal
    deriving (Eq, Show)

class Binary a where
    toBin :: a -> BinOp

instance Binary NumOp where
    toBin = BNum

instance Binary CompOp where
    toBin = BComp

instance Binary BitOp where
    toBin = BBit

eBinary :: Binary a => a -> Expr -> Expr -> Expr
eBinary = EBinary . toBin

-- * Statements

{-| Assigning operations, possibly with arithmetical/boolean modification. -}
data AssignOp
    = AAssign | AModify NumOp
    deriving (Eq, Show)

{-| Statement (instruction). -}
data Stmt
    = SExpression Expr -- ^ Calling an expression (typically a function/script with side effects)
    -- Variable declaration and modification
    | SDeclare [(Name, Maybe Expr)]  -- ^ Declaring local variable(s)
    | SAssign Variable AssignOp Expr -- ^ Assigning or modifying an existing variable
    -- Control flow structures
    | SBlock   [Stmt]          -- ^ Nested sequence of statements
    | SWith    Expr Stmt       -- ^ Switching the execution context into an another instance
    | SRepeat  Expr Stmt       -- ^ Repeating some instructions several times
    | SWhile   Expr Stmt       -- ^ Loop with a pre-condition
    | SDoUntil Stmt Expr       -- ^ Loop with a post-condition
    | SFor    Stmt Expr Stmt Stmt    -- ^ For loop. TODO: limit the first header stmt to assign or declare, and the second one to assign
    | SIf     Expr Stmt (Maybe Stmt) -- ^ Conditional. If the `else` branch is missing, the second statement is [Nothing].
    | SSwitch Expr [([Expr], Stmt)]  -- ^ Switch-case
    -- Control flow redirection
    | SBreak       -- ^ Break from a loop or switch-case
    | SContinue    -- ^ Continue to the next loop iteration
    | SExit        -- ^ Exit from a script/event without a result
    | SReturn Expr -- ^ Return the result from a script
    deriving (Eq, Show)

{-| Any GML source is a list of statements. -}
type Program = [Stmt]
