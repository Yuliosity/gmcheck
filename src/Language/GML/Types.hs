{-|
Module      : Language.GML.Types
Description : GML Types

Type system of GML values.
-}

{-# LANGUAGE PatternSynonyms #-}

module Language.GML.Types where

import Data.List (union)
{-| Identifier (name). -}
type Name = String

{-| Linear data structure type. In GML any structure descriptor
    is also just a number, but here we want to differ. -}
data Container
    = SArray
    | SStack
    | SList
    | SMap --CHECKME: polymorphic by key?
    | SQueue
    | SPriorityQueue
    deriving (Eq, Show)

{-| 2D data container type. -}
data Container2
    = SArray2 -- ^ Deprecated in GMS 2.3.
    | SGrid
    deriving (Eq, Show)

-- data AnyContainer = AnyC1 Container | AnyC2 Container2

{-| Value type. -}
data Type
    = TUnknown [Type] -- ^ Unknown type with possibilities, if any
    -- Base types
    | TVoid   -- ^ GML 'undefined'
    | TReal   -- ^ GML number, a primitive type
    | TString -- ^ GML string, a primitive type
    | TPtr    -- ^ GML pointer, a primitive type
    | TMatrix -- ^ GML pointer, a primitive type
    -- Derived types
    | TNewtype String -- ^ Represented as just a number, but distinguished here
    -- Vector types
    | TContainer  Container  Type -- ^ Linear container of typed values.
    | TContainer2 Container2 Type -- ^ Two-dimensional container of typed values.
    deriving (Eq, Show)

indexType :: Container -> Type
indexType = \case
    SArray -> TInt
    SList  -> TInt
    SMap   -> TAny
    _      -> TVoid

isSubtype :: Type -> Type -> Bool
isSubtype _ TAny = True
isSubtype t1 (TUnknown ts) = t1 `elem` ts
isSubtype TInt TReal = True
isSubtype t1 t2 | t1 == t2 = True
isSubtype _ _ = False

{- |Unknown type. -}
pattern TAny :: Type
pattern TAny = TUnknown []

instance Semigroup Type where
    TAny <> t2   = t2
    t1   <> TAny = t1
    TUnknown t1 <> TUnknown t2 = TUnknown $  t1  `union` t2
    TUnknown t1 <> t2          = TUnknown $ [t2] `union` t1
    t1          <> TUnknown t2 = TUnknown $ [t1] `union` t2
    t1 <> t2 = TUnknown [t1, t2]

instance Monoid Type where
    mempty = TAny

{-| Boolean. In GML VM, `true` is just any real value which is greater than 0.5.
    Maybe some more typechecking will be added to that later. -}
pattern TBool :: Type
pattern TBool = TReal

{-| Integer. In GML VM, there is no separate type for integral values.
    Reserved for future typechecking. -}
pattern TInt :: Type
pattern TInt = TReal

{-| Character. In GML, single-character strings are used for that.
    Reserved for future typechecking. -}
pattern TChar :: Type
pattern TChar = TString

{-| Real value between 0 and 1. Reserved for future typechecking. -}
pattern TAlpha :: Type
pattern TAlpha = TReal

{-| Instance descriptor. Reserved for future typechecking. -}
pattern TInstance :: Type 
pattern TInstance = TReal

{-| Data structure descriptors. -}
pattern TArray, TList, TMap, TPriorityQueue, TQueue, TStack :: Type -> Type
{-| One-dimensional array of values. -}
pattern TArray t = TContainer SArray t
pattern TList  t = TContainer SList t
pattern TMap   t = TContainer SMap t
pattern TPriorityQueue t = TContainer SPriorityQueue t
pattern TQueue t = TContainer SQueue t
pattern TStack t = TContainer SStack t

pattern TArray2, TGrid :: Type -> Type
{-| Two-dimensional array of values. Legacy in GMS 2.3+. -}
pattern TArray2 t = TContainer2 SArray2 t
pattern TGrid t = TContainer2 SGrid t

{-| Possibly named function argument. -}
type Argument = (Name, Type)

{-| Optional or variadic arguments (cannot be mixed). -}
data MoreArgs
    = OptArgs [Argument] -- ^ Optional arguments
    | VarArgs  Argument  -- ^ Variadic arguments
    deriving (Eq, Show)

{-| Function or script signature. -}
data Signature = Signature
    [Argument]       -- ^ Mandatory arguments
    MoreArgs         -- ^ Optional or variadic arguments
    Type             -- ^ Return type
    deriving (Eq, Show)

allArgs :: Signature -> [Argument]
allArgs (Signature args more _) = take maxGmlArgs $ args ++ case more of
    OptArgs opt -> opt
    VarArgs (name, ty) -> map (\i -> (name ++ show i, ty)) [0 ..]
    where
        maxGmlArgs = 16

minArgs, maxArgs :: Signature -> Int
minArgs (Signature args _ _) = length args
maxArgs sig = length $ allArgs sig

{-| Enumeration of named constants. -}
data Enum = Enum !String ![(String, Int)]
