{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Language.GML.AST
Description : GML AST

Everything representing the Game Maker Language source tree.
-}
module Language.GML.AST (
    module Language.GML.Location,
    module Language.GML.AST,
) where

import Data.String
import Data.Text (Text)

import Language.GML.Location
import Language.GML.Types

-- * GML values

-- | Variables that hold a value and may be read or changed.
data Variable
    = -- | Local, self or global variable
      VVar Name
    | -- | Field/instance variable (possibly chained)
      VField Variable Name
    | -- | Data structure accessor. Arrays are a special case.
      VContainer Container Variable Expr
    | -- | 2D data structure accessor
      VContainer2 Container2 Variable (Expr, Expr)
    deriving (Eq, Show)

-- | Prepend a qualifier for a variable, e.g. a[2] -> c.a[2]
qualify :: Name -> Variable -> Variable
qualify obj = \case
    VVar name -> VField (VVar obj) name
    VField var name -> VField (qualify obj var) name
    VContainer c var expr -> VContainer c (qualify obj var) expr
    VContainer2 c var expr -> VContainer2 c (qualify obj var) expr

-- | One-dimensional array, indexed by a number: `a[b]`.
pattern VArray v e = VContainer SArray v e

-- | Two-dimensional array, indexed by two numbers: `a[b, c]`. Legacy in GMS 2.3.
pattern VArray2 v e = VContainer2 SArray2 v e

-- * Operators

-- | Arithetical and logical operations, used in both modification assignment and binary operations.
data BinOp
    = -- | Addition, `x + `y`
      Add
    | -- | Subtraction, `x - y`
      Sub
    | -- | Multiplication, `x * y`
      Mul
    | -- | Division, `x / y`
      Div
    | -- | Modulus, `x % y` or `x mod y`
      Mod
    | -- | Integral division, `x div y`
      IntDiv
    | -- | Bit shift right, `x << y`
      Shr
    | -- | Bit shift left, `x >> y`
      Shl
    | -- | Bitwise and, `x & y`
      BitAnd
    | -- | Bitwise or, `x | y`
      BitOr
    | -- | Bitwise xor, `CHECK`
      BitXor
    | -- | Boolean operations.
      And
    | -- \^ Logical AND, `x && y` or `x and y`

      -- | Logical OR, `x || y` or `x or y`
      Or
    | -- | Logical XOR, `x ^^ y`
      Xor
    | -- | Comparison operators.
      Eq
    | -- \^ Equality: `a == b` (or `a = b` in expression context)

      -- | Unequality: `a != b`
      NotEq
    | -- | Less than: `a < b`
      Less
    | -- | Greater than: `a > b`
      Greater
    | -- | Less or equal: `a <= b`
      LessEq
    | -- | Greater or equal: `a >= b`
      GreaterEq
    | -- | Nullish operators.
      Nullish
    -- \^ Null coalesce, `a ?? b`
    deriving (Eq, Show)

-- | Modify operators, in assignments.
data ModifyOp
    = -- | Addition, `x += `y`
      MAdd
    | -- | Subtraction, `x -= y`
      MSub
    | -- | Multiplication, `x *= y`
      MMul
    | -- | Division, `x /= y`
      MDiv
    | -- | Bitwise and, `x &= y`
      MBitAnd
    | -- | Bitwise or, `x |= y`
      MBitOr
    | -- | Null coalesce, `x ??= y`
      MNullish
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

-- | Unary operators, in order of precedence.
data UnOp
    = -- | Bit negation: `~a`
      UBitNeg
    | -- | Arithmetical negation: `-a`
      UNeg
    | -- | Boolean negation: `!a`
      UNot
    | -- | Prefix increment: `++a`
      UPreInc
    | -- | Prefix decrement: `--a`
      UPreDec
    | -- | Postfix increment: `a++`
      UPostInc
    | -- | Postfix decrement: `a--`
      UPostDec
    deriving (Eq, Show)

-- * Expressions

-- | Anonymous function (possibly a constructor) with arguments and a body.
data Function = Function [Name] FunctionKind Block
    deriving (Eq, Show)

-- | A plain function or a possibly inherited constructor.
data FunctionKind = PlainFunction | Constructor (Maybe Funcall)
    deriving (Eq, Show)

-- | Function/constructor call. |
type Funcall = (Name, [Expr])

-- | Expression which can be evaluated to a value.
data Expr
    = -- Values
      EVariable (Located Variable)
    | -- | Undefined literal: `undefined`
      EUndefined
    | -- | Boolean literal: `true`, `false`
      EBool Bool
    | -- | TODO: pointers: `pointer_null`
      EPointer
    | -- | Numeric literal: `2`, `3.141`
      ENumber Double
    | -- | String literal: `"string"`
      EString Text
    | -- | Array literal: `[1, 2, 3]`
      EArray [Expr]
    | -- | Inline function: `function (arg) {body}`
      EFunction Function
    | -- | Struct: `{a: 1, b: "str"}`
      -- Operators
      EStruct [(FieldName, Expr)]
    | -- | Unary expression
      EUnary UnOp Expr
    | -- | Binary expression
      EBinary BinOp Expr Expr
    | -- | Ternary conditional `cond ? t : f`
      ETernary Expr Expr Expr
    | -- | Function/script call with arguments
      EFuncall Funcall
    | -- | Constructor call with arguments
      ENew Funcall
    deriving (Eq, Show)

-- Helper instances for writing expressions in code

instance IsString Variable where
    fromString = VVar . fromString -- TODO: parse

instance IsString Expr where
    fromString str = EVariable $ Located zeroPos (fromString str)

instance (IsString a) => IsString (Located a) where
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

-- | Statement (instruction).
data Stmt
    = -- | Calling an expression (typically a function/script with side effects)
      -- Declarations and modification
      SExpression Expr
    | -- | Declaring local variable(s) with `var`
      SDeclare [(Name, Maybe Expr)]
    | -- | Assigning a variable with `=`, possibly declaring it in-place
      SAssign (Located Variable) Expr
    | -- | Modifying an existing variable with an operator like `+=` or `^=`
      SModify ModifyOp (Located Variable) Expr
    | -- | Declaring a function (possibly constructor) with arguments and a body
      SFunction Name Function
    | -- | Delete operator
      SDelete Name
    | -- | Enum: `enum foo { a, b, c }`
      -- Control flow structures
      SEnum Name [FieldName]
    | -- | Nested sequence of statements
      SBlock Block
    | -- | Switching the execution context into an another instance
      SWith Expr Stmt
    | -- | `repeat`ing some instructions several times
      SRepeat Expr Stmt
    | -- | Loop with a pre-condition
      SWhile Expr Stmt
    | -- | Loop with a post-condition
      SDoUntil Stmt Expr
    | -- | [for] loop. TODO: limit the first header stmt to assign or declare, and the second one to assign
      SFor Stmt Expr Stmt Stmt
    | -- | `if` conditional, with mandatory `then` branch and optional `else` branch
      SIf Expr Stmt (Maybe Stmt)
    | -- | Switch-case. For the default branch, the case list is empty
      SSwitch Expr [([Expr], Block)]
    | -- | `try` block, with optional `catch` and optional `finally` blocks
      -- Control flow redirection
      STry Block (Maybe (Name, Block)) (Maybe Block)
    | -- | `break` from a loop or `switch`-`case`
      SBreak
    | -- | `continue` to the next loop iteration
      SContinue
    | -- | `exit` from a script/event without a result
      SExit
    | -- | `return` the result from a function
      SReturn Expr
    | -- | `throw` an exception
      SThrow Expr
    deriving (Eq, Show)

-- | A block is a sequence of statements, typically in braces.
type Block = [Stmt]

-- | Any GML source is a list of statements.
type Program = Block
