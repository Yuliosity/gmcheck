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

{-| Resource type. In GML any resource descriptor is actually
    just a number, but here we want to differ. -}
data Resource
    = RBackground
    | RFont
    | RObject
    | RPath
    | RRoom
    | RSound
    | RSprite
    -- TODO: paths, etc.
    deriving (Eq, Show)

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
    | TVoid -- ^ GML 'undefined'
    | TReal -- ^ GML number, a primitive type
    | TString -- ^ GML string, a primitive type
    -- Derived types
    | TColor -- ^ RGB color. Represented as just a number in GML.
    | TEnum String -- ^ Enumeration with a label
    | TId Resource -- ^ Resource descriptor. Represented as just a number in GML.
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

{-| Keyboard key code enum. Reserved for future typechecking. -}
pattern TKeyCode :: Type
pattern TKeyCode = TInt

{-| Mouse button enum. Reserved for future typechecking. -}
pattern TMouseButton :: Type
pattern TMouseButton = TInt

{-| Instance descriptor. Reserved for future typechecking. -}
pattern TInstance :: Type 
pattern TInstance = TReal

{-| Resource descriptors. -}
pattern TBackground, TFont, TObject, TPath, TRoom, TSound, TSprite :: Type
pattern TBackground = TId RBackground
pattern TFont   = TId RFont
pattern TObject = TId RObject
pattern TPath   = TId RPath
pattern TRoom   = TId RRoom
pattern TSound  = TId RSound
pattern TSprite = TId RSprite

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

{-| Function or script signature. -}
data Signature =
    [Argument] -- ^ Argument types
    :-> 
    Type -- ^ Return type
    --TODO: variadic and optional arguments
    deriving (Eq, Show)

{-| Enumeration of named constants. -}
data Enum = Enum !String ![(String, Int)]
