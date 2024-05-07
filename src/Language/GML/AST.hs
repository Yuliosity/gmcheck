{-|
Module      : Language.GML.AST
Description : GML AST

Everything representing the Game Maker Language source tree.
-}

{-# LANGUAGE PatternSynonyms #-}

module Language.GML.AST
    ( module Language.GML.Location
    , module Language.GML.AST
    ) where

import Data.String
import Data.Text (Text)

import Language.GML.Location
import Language.GML.Types

-- * GML values

{-| Variables that hold a value and may be read or changed. -}
data Variable
    = VVar   Name          -- ^ Local, self or global variable
    | VField Variable Name -- ^ Field/instance variable (possibly chained)
    | VContainer  Container  Variable Expr         -- ^ Data structure accessor. Arrays are a special case.
    | VContainer2 Container2 Variable (Expr, Expr) -- ^ 2D data structure accessor
    deriving (Eq, Show)

{-| Prepend a qualifier for a variable, e.g. a[2] -> c.a[2] -}
qualify :: Name -> Variable -> Variable
qualify obj = \case
    VVar name              -> VField (VVar obj) name
    VField var name        -> VField (qualify obj var) name
    VContainer  c var expr -> VContainer  c (qualify obj var) expr
    VContainer2 c var expr -> VContainer2 c (qualify obj var) expr

{-| One-dimensional array, indexed by a number: `a[b]`. -}
pattern VArray  v e = VContainer  SArray  v e

{-| Two-dimensional array, indexed by two numbers: `a[b, c]`. Legacy in GMS 2.3. -}
pattern VArray2 v e = VContainer2 SArray2 v e

-- * Operators

{-| Arithetical and logical operations, used in both modification assignment and binary operations. -}
data BinOp
    = Add    -- ^ Addition, `x + `y`
    | Sub    -- ^ Subtraction, `x - y`
    | Mul    -- ^ Multiplication, `x * y`
    | Div    -- ^ Division, `x / y`
    | Mod    -- ^ Modulus, `x % y` or `x mod y`
    | IntDiv -- ^ Integral division, `x div y`
    | Shr    -- ^ Bit shift right, `x << y`
    | Shl    -- ^ Bit shift left, `x >> y`
    | BitAnd -- ^ Bitwise and, `x & y`
    | BitOr  -- ^ Bitwise or, `x | y`
    | BitXor -- ^ Bitwise xor, `CHECK`
    {-| Boolean operations. -}
    | And -- ^ Logical AND, `x && y` or `x and y`
    | Or  -- ^ Logical OR, `x || y` or `x or y`
    | Xor -- ^ Logical XOR, `x ^^ y`
    {-| Comparison operators. -}
    | Eq        -- ^ Equality: `a == b` (or `a = b` in expression context)
    | NotEq     -- ^ Unequality: `a != b`
    | Less      -- ^ Less than: `a < b`
    | Greater   -- ^ Greater than: `a > b`
    | LessEq    -- ^ Less or equal: `a <= b`
    | GreaterEq -- ^ Greater or equal: `a >= b`
    {-| Nullish operators. -}
    | Nullish   -- ^ Null coalesce, `a ?? b`
    deriving (Eq, Show)

{-| Modify operators, in assignments. -}
data ModifyOp
    = MAdd -- ^ Addition, `x += `y`
    | MSub -- ^ Subtraction, `x -= y`
    | MMul -- ^ Multiplication, `x *= y`
    | MDiv -- ^ Division, `x /= y`
    | MBitAnd -- ^ Bitwise and, `x &= y`
    | MBitOr  -- ^ Bitwise or, `x |= y`
    | MNullish -- ^ Null coalesce, `x ??= y`
    deriving (Eq, Show)

modifyToBin :: ModifyOp -> BinOp
modifyToBin = \case
    MAdd -> Add
    MSub -> Sub
    MMul -> Mul
    MDiv -> Div
    MBitAnd -> BitAnd
    MBitOr -> BitOr
    MNullish -> Nullish

{-| Unary operators, in order of precedence. -}
data UnOp
    = UBitNeg  -- ^ Bit negation: `~a`
    | UNeg     -- ^ Arithmetical negation: `-a`
    | UNot     -- ^ Boolean negation: `!a`
    | UPreInc  -- ^ Prefix increment: `++a`
    | UPreDec  -- ^ Prefix decrement: `--a`
    | UPostInc -- ^ Postfix increment: `a++`
    | UPostDec -- ^ Postfix decrement: `a--`
    deriving (Eq, Show)

-- * Expressions

{-| Anonymous function (possibly a constructor) with arguments and a body. -}
data Function = Function [Name] FunctionKind Block
    deriving (Eq, Show)

{-| A plain function or a possibly inherited constructor. -}
data FunctionKind = PlainFunction | Constructor (Maybe Funcall)
    deriving (Eq, Show)

{-| Function/constructor call. |-}
type Funcall = (Name, [Expr])

{-| Expression which can be evaluated to a value. -}
data Expr
    -- Values
    = EVariable (Located Variable)
    | EUndefined                -- ^ Undefined literal: `undefined`
    | EBool     Bool            -- ^ Boolean literal: `true`, `false`
    | EPointer                  -- ^ TODO: pointers: `pointer_null`
    | ENumber   Double          -- ^ Numeric literal: `2`, `3.141`
    | EString   Text            -- ^ String literal: `"string"`
    | EArray    [Expr]          -- ^ Array literal: `[1, 2, 3]`
    | EFunction Function        -- ^ Inline function: `function (arg) {body}`
    | EStruct   [(FieldName, Expr)] -- ^ Struct: `{a: 1, b: "str"}`
    -- Operators
    | EUnary    UnOp  Expr      -- ^ Unary expression
    | EBinary   BinOp Expr Expr -- ^ Binary expression
    | ETernary  Expr  Expr Expr -- ^ Ternary conditional `cond ? t : f`
    | EFuncall  Funcall     -- ^ Function/script call with arguments
    | ENew      Funcall     -- ^ Constructor call with arguments
    deriving (Eq, Show)

-- Helper instances for writing expressions in code

instance IsString Variable where
    fromString = VVar . fromString --TODO: parse

instance IsString Expr where
    fromString str = EVariable $ Located zeroPos (fromString str) 

instance IsString a => IsString (Located a) where
    fromString = Located zeroPos . fromString

instance Num Expr where
    fromInteger = ENumber . fromInteger
    (+) = EBinary Add
    (-) = EBinary Sub
    (*) = EBinary Mul
    negate = EUnary UNeg
    abs x = EFuncall ("abs", [x])
    signum x = EFuncall ("sign", [x])

instance Fractional Expr where
    fromRational = ENumber . fromRational
    (/) = EBinary Div

-- * Statements

{-| Statement (instruction). -}
data Stmt
    = SExpression Expr -- ^ Calling an expression (typically a function/script with side effects)
    -- Declarations and modification
    | SDeclare [(Name, Maybe Expr)] -- ^ Declaring local variable(s) with `var`
    | SAssign (Located Variable) Expr -- ^ Assigning a variable with `=`, possibly declaring it in-place
    | SModify ModifyOp (Located Variable) Expr   -- ^ Modifying an existing variable with an operator like `+=` or `^=`
    | SFunction Name Function       -- ^ Declaring a function (possibly constructor) with arguments and a body
    | SDelete Name                  -- ^ Delete operator
    | SEnum     Name [FieldName]    -- ^ Enum: `enum foo { a, b, c }`
    -- Control flow structures
    | SBlock   Block           -- ^ Nested sequence of statements
    | SWith    Expr Stmt       -- ^ Switching the execution context into an another instance
    | SRepeat  Expr Stmt       -- ^ `repeat`ing some instructions several times
    | SWhile   Expr Stmt       -- ^ Loop with a pre-condition
    | SDoUntil Stmt Expr       -- ^ Loop with a post-condition
    | SFor     Stmt Expr Stmt Stmt    -- ^ [for] loop. TODO: limit the first header stmt to assign or declare, and the second one to assign
    | SIf      Expr Stmt (Maybe Stmt) -- ^ `if` conditional, with mandatory `then` branch and optional `else` branch
    | SSwitch  Expr [([Expr], Block)] -- ^ Switch-case. For the default branch, the case list is empty
    | STry     Block (Maybe (Name, Block)) (Maybe Block) -- ^ `try` block, with optional `catch` and optional `finally` blocks
    -- Control flow redirection
    | SBreak       -- ^ `break` from a loop or `switch`-`case`
    | SContinue    -- ^ `continue` to the next loop iteration
    | SExit        -- ^ `exit` from a script/event without a result
    | SReturn Expr -- ^ `return` the result from a function
    | SThrow  Expr -- ^ `throw` an exception
    deriving (Eq, Show)

{-| A block is a sequence of statements, typically in braces. -}
type Block = [Stmt]

{-| Any GML source is a list of statements. -}
type Program = Block
