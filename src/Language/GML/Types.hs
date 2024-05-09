{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Language.GML.Types
Description : GML Types

Type system of GML values.
-}
module Language.GML.Types where

import Data.List (union)
import Data.Text (Text, pack)

-- | Identifier (name).
type Name = Text

-- | Name of struct fields.
type FieldName = Text

{- | Linear data structure type. In GML any structure descriptor
    is also just a number, but here we want to differ.
-}
data Container
    = SArray
    | SStack
    | SList
    | SMap -- CHECKME: polymorphic by key?
    | SQueue
    | SPriorityQueue
    deriving (Eq, Show)

-- | 2D data container type.
data Container2
    = -- | Deprecated in GMS 2.3.
      SArray2
    | SGrid
    deriving (Eq, Show)

-- data AnyContainer = AnyC1 Container | AnyC2 Container2

-- | Value type.
data Type
    = -- | Unknown type with possibilities, if any
      -- Base types
      TUnknown [Type]
    | -- | GML 'undefined'
      TVoid
    | -- | GML number, a primitive type
      TReal
    | -- | GML string, a primitive type
      TString
    | -- | GML pointer, a primitive type
      TPtr
    | -- | GML matrix, a primitive type
      TMatrix
    | -- | GML pointer, a primitive type
      TPointer
    | -- | GML struct, a primitive type
      TStruct [(FieldName, Type)]
    | -- | GML inline function, a primitive type
      -- Derived types
      TFunction [Argument] Type
    | -- | Represented as just a number, but distinguished here
      -- Vector types
      TNewtype Text
    | -- | Linear container of typed values.
      TContainer Container Type
    | -- | Two-dimensional container of typed values.
      -- Generic types, only in function signatures
      TContainer2 Container2 Type
    | TTypeVar Name
    deriving (Eq, Show)

-- FIXME: compare functions ignoring argument names

indexType :: Container -> Type
indexType = \case
    SArray -> TInt
    SList -> TInt
    SMap -> TAny
    _ -> TVoid

isSubtype :: Type -> Type -> Bool
isSubtype _ TAny = True
isSubtype t1 (TUnknown ts) = t1 `elem` ts
isSubtype TInt TReal = True
isSubtype t1 t2 | t1 == t2 = True
isSubtype _ _ = False

-- | Unknown type.
pattern TAny :: Type
pattern TAny = TUnknown []

instance Semigroup Type where
    TAny <> t2 = t2
    t1 <> TAny = t1
    TUnknown t1 <> TUnknown t2 = TUnknown $ t1 `union` t2
    TUnknown t1 <> t2 = TUnknown $ [t2] `union` t1
    t1 <> TUnknown t2 = TUnknown $ [t1] `union` t2
    t1 <> t2 = TUnknown [t1, t2]

instance Monoid Type where
    mempty = TAny

{- | Boolean. In GML VM, `true` is just any real value which is greater than 0.5.
    Maybe some more typechecking will be added to that later.
-}
pattern TBool :: Type
pattern TBool = TReal

{- | Integer. In GML VM, there is no separate type for integral values.
    Reserved for future typechecking.
-}
pattern TInt :: Type
pattern TInt = TReal

{- | Character. In GML, single-character strings are used for that.
    Reserved for future typechecking.
-}
pattern TChar :: Type
pattern TChar = TString

-- | Real value between 0 and 1. Reserved for future typechecking.
pattern TAlpha :: Type
pattern TAlpha = TReal

-- | Instance descriptor. Reserved for future typechecking.
pattern TInstance :: Type
pattern TInstance = TReal

-- | Data structure descriptors.
pattern TArray, TList, TMap, TPriorityQueue, TQueue, TStack :: Type -> Type

-- | One-dimensional array of values.
pattern TArray t = TContainer SArray t

pattern TList t = TContainer SList t
pattern TMap t = TContainer SMap t
pattern TPriorityQueue t = TContainer SPriorityQueue t
pattern TQueue t = TContainer SQueue t
pattern TStack t = TContainer SStack t

pattern TArray2, TGrid :: Type -> Type

-- | Two-dimensional array of values. Legacy in GMS 2.3+.
pattern TArray2 t = TContainer2 SArray2 t

pattern TGrid t = TContainer2 SGrid t

-- | Runtime exception.
pattern TException =
    TStruct
        [ ("message", TString)
            , ("longMessage", TString)
            , ("script", TString)
            , ("stacktrace", TArray TString)
            ]

-- | Possibly named function argument.
type Argument = (Name, Type)

-- | Optional or variadic arguments (cannot be mixed).
data MoreArgs
    = -- | Optional arguments
      OptArgs [Argument]
    | -- | Variadic arguments
      VarArgs Argument
    deriving (Eq, Show)

-- | Function or script signature.
data Signature
    = Signature
        -- | Mandatory arguments
        [Argument]
        -- | Optional or variadic arguments
        MoreArgs
        -- | Return type
        Type
    deriving (Eq, Show)

pattern (:->) args ret = Signature args (OptArgs []) ret

allArgs :: Signature -> [Argument]
allArgs (Signature args more _) =
    take maxGmlArgs $
        args ++ case more of
            OptArgs opt -> opt
            VarArgs (name, ty) -> map (\i -> (name <> pack (show i), ty)) [0 ..]
  where
    maxGmlArgs = 16

minArgs, maxArgs :: Signature -> Int
minArgs (Signature args _ _) = length args
maxArgs sig = length $ allArgs sig

-- | Enumeration of named constants.
data Enum = Enum !Name ![(Name, Int)]
