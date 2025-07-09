{-|
Module      : Language.GML.AST
Description : GML AST

Everything representing the Game Maker Language source tree.
-}

{-# LANGUAGE FlexibleInstances, PatternSynonyms #-}

module Language.GML.AST
    ( module Language.GML.Location
    , module Language.GML.AST
    ) where

import Data.String (IsString(..))
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

type VarLoc = Located Variable

{-| Keywords for special instances. -}
pattern ISelf, IOther, INoone :: Variable
pattern ISelf  = VVar "self"
pattern IOther = VVar "other"
pattern INoone = VVar "noone"

{-| Prepend a qualifier for a variable, e.g. @a[2]@ -> @c.a[2]@ -}
qualify :: Name -> Variable -> Variable
qualify obj = \case
    VVar name              -> VField (VVar obj) name
    VField var name        -> VField (qualify obj var) name
    VContainer  c var expr -> VContainer  c (qualify obj var) expr
    VContainer2 c var expr -> VContainer2 c (qualify obj var) expr

{-| One-dimensional array, indexed by a number: @a[b]@. -}
pattern VArray  v e = VContainer  SArray  v e

{-| Two-dimensional array, indexed by two numbers: @a[b, c]@. Legacy in GMS 2.3. -}
pattern VArray2 v e = VContainer2 SArray2 v e

-- * Operators

{-| Arithmetical and logical operations, used in binary expressions. -}
data BinOp
    = Add    -- ^ Addition, @x + y@
    | Sub    -- ^ Subtraction, @x - y@
    | Mul    -- ^ Multiplication, @x * y@
    | Div    -- ^ Division, @x / y@
    | Mod    -- ^ Modulus, @x % y@ or @x mod y@
    | IntDiv -- ^ Integral division, @x div y@
    | Shr    -- ^ Bit shift right, @x << y@
    | Shl    -- ^ Bit shift left, @x >> y@
    | BitAnd -- ^ Bitwise and, @x & y@
    | BitOr  -- ^ Bitwise or, @x | y@
    | BitXor -- ^ Bitwise xor, @CHECK@
    -- Boolean operations
    | And -- ^ Logical AND, @x && y@ or @x and y@
    | Or  -- ^ Logical OR, @x || y@ or @x or y@
    | Xor -- ^ Logical XOR, @x ^^ y@
    -- Comparison operators
    | Eq        -- ^ Equality: @a == b@ (or @a = b@ in expression context)
    | NotEq     -- ^ Unequality: @a != b@
    | Less      -- ^ Less than: @a < b@
    | Greater   -- ^ Greater than: @a > b@
    | LessEq    -- ^ Less or equal: @a <= b@
    | GreaterEq -- ^ Greater or equal: @a >= b@
    -- Nullish operators
    | Nullish   -- ^ Null coalesce, @a ?? b@
    deriving (Eq, Show)

{-| Modify operators, used in assignments. -}
data ModifyOp
    = MAdd -- ^ Addition, @x += y@
    | MSub -- ^ Subtraction, @x -= y@
    | MMul -- ^ Multiplication, @x *= y@
    | MDiv -- ^ Division, @x /= y@
    | MBitAnd -- ^ Bitwise and, @x &= y@
    | MBitOr  -- ^ Bitwise or, @x |= y@
    | MNullish -- ^ Null coalesce, @x ??= y@
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
    = UBitNeg  -- ^ Bit negation: @~a@
    | UPos     -- ^ Arithmetical positive: @+a@
    | UNeg     -- ^ Arithmetical negation: @-a@
    | UNot     -- ^ Boolean negation: @!a@
    | UPreInc  -- ^ Prefix increment: @++a@
    | UPreDec  -- ^ Prefix decrement: @--a@
    | UPostInc -- ^ Postfix increment: @a++@
    | UPostDec -- ^ Postfix decrement: @a--@
    deriving (Eq, Show)

-- * Expressions

{-| Anonymous function (possibly a constructor) with arguments and a body. -}
data Function = Function
    { fArgs :: [VarDecl]
    , fKind :: FunctionKind
    , fBody :: Block
    }
    deriving (Eq, Show)

{-| A plain function or a possibly inherited constructor. -}
data FunctionKind = PlainFunction | Constructor (Maybe (Name, [Expr]))
    deriving (Eq, Show)

{-| Expressions which can be evaluated to a value. -}
data Expr_
    -- Values
    = EVariable Variable        -- ^ Variable: @foo@, @bar.baz@, @a[2]@
    | EUndefined                -- ^ Undefined literal: @undefined@
    | EBool     Bool            -- ^ Boolean literal: @true@, @false@
    | EPointer                  -- ^ TODO: pointers: @pointer_null@
    | ENumber   Double          -- ^ Numeric literal: @2@, @3.141@
    | EString   Text            -- ^ String literal: @"string"@
    | EArray    [Expr]          -- ^ Array literal: @[1, 2, 3]@
    | EFunction Function        -- ^ Inline function: @function (arg) {body}@
    | EStruct   [(FieldName, Expr)] -- ^ Struct: @{a: 1, b: "str"}@
    -- Operators
    | EUnary    UnOp  Expr      -- ^ Unary expression
    | EBinary   BinOp Expr Expr -- ^ Binary expression
    | ETernary  Expr  Expr Expr -- ^ Ternary conditional @cond ? t : f@
    | EFuncall  Variable [Expr] -- ^ Function/script call with arguments
    | ENew      Variable [Expr] -- ^ Constructor call with arguments
    deriving (Eq, Show)

type Expr = Located Expr_

-- Helper instances for writing expressions in code

instance IsString Variable where
    fromString = VVar . fromString --TODO: parse

instance IsString Expr_ where
    fromString = EString . fromString

instance IsString a => IsString (Located a) where
    fromString = (:@ zeroPos) . fromString

instance Num Expr where
    fromInteger x = ENumber (fromInteger x) :@ zeroPos
    a + b = EBinary Add a b :@ getPos a
    a - b = EBinary Sub a b :@ getPos a
    a * b = EBinary Mul a b :@ getPos a
    negate x = EUnary UNeg x :@ getPos x
    abs x = EFuncall "abs" [x] :@ getPos x
    signum x = EFuncall "sign" [x] :@ getPos x

instance Fractional Expr where
    fromRational = (:@ zeroPos) . ENumber . fromRational
    a / b =  EBinary Div a b :@ getPos a

-- * Statements

data VarDecl = VarDecl
    { vdName :: Name       -- ^ Variable name
    , vdExpr :: Maybe Expr -- ^ Optional initializer
    , vdType :: Maybe Type -- ^ Optional type annotation
    }
    deriving (Eq, Show)

pattern SimpleVarDecl :: Name -> VarDecl
pattern SimpleVarDecl name = VarDecl name Nothing Nothing

pattern InitVarDecl :: Name -> Expr -> VarDecl
pattern InitVarDecl name expr = VarDecl name (Just expr) Nothing

instance IsString VarDecl where
    fromString = SimpleVarDecl . fromString

{-| Statement (instruction). -}
data Stmt
    = SExpression Expr    -- ^ Calling an expression (typically a function/script with side effects)
    -- Declarations and modification
    | SDeclare [VarDecl]  -- ^ Declaring local variable(s) with @var@
    | SGlobalvar VarDecl  -- ^ Declaring a global variable
    | SStatic VarDecl     -- ^ Declaring a static variable
    | SAssign VarLoc Expr -- ^ Assigning a variable with @=@, possibly declaring it in-place
    | SModify ModifyOp VarLoc Expr    -- ^ Modifying an existing variable with an operator like @+=@ or @^=@
    | SFunction Name Function         -- ^ Declaring a function (possibly constructor) with arguments and a body
    | SDelete Name                    -- ^ Delete operator
    | SEnum     Name [FieldName]      -- ^ Enum: @enum foo { a, b, c }@
    -- Control flow structures
    | SBlock   Block                  -- ^ Nested sequence of statements
    | SWith    Expr Stmt              -- ^ Switching the execution context into an another instance
    | SRepeat  Expr Stmt              -- ^ @repeat@ing some instructions several times
    | SWhile   Expr Stmt              -- ^ Loop with a pre-condition
    | SDoUntil Stmt Expr              -- ^ Loop with a post-condition
    | SFor     Stmt Expr Stmt Stmt    -- ^ @for@ loop. TODO: limit the first header stmt to assign or declare, and the second one to assign
    | SIf      Expr Stmt (Maybe Stmt) -- ^ @if@ conditional, with mandatory @then@ branch and optional @else@ branch
    | SSwitch  Expr [([Expr], Block)] -- ^ Switch-case. For the default branch, the case list is empty
    | STry     Block (Maybe (Name, Block)) (Maybe Block) -- ^ @try@ block, with optional @catch@ and optional @finally@ blocks
    -- Control flow redirection
    | SBreak       -- ^ @break@ from a loop or @switch@-@case@
    | SContinue    -- ^ @continue@ to the next loop iteration
    | SExit        -- ^ @exit@ from a script/event without a result
    | SThrow  Expr -- ^ @throw@ an exception
    | SReturn Expr -- ^ @return@ the result from a function
    | SReturnVoid  -- ^ @return@ without a result
    deriving (Eq, Show)

{-| A block is a sequence of statements, typically in braces. -}
type Block = [Stmt]

{-| Any GML source is a list of statements. -}
type Program = Block
