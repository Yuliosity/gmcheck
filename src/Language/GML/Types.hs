{-|
Module      : Types
Description : GML Types

Type system of GML values.
-}

module Language.GML.Types where

import Data.Monoid

import Language.GML.AST (Name)

{-| Resource type. In GML any resource descriptor actually just a number, but here we want to differ. -}
data Resource
    = RSprite
    | RBackground
    | RSound
    | RObject
    | RRoom
    -- TODO: paths, etc.
    deriving (Eq, Show)

{-| Data structure type. In GML any structure descriptor is also just a number, but here we want to differ. -}
data Structure
    = SStack
    | SList
    | SGrid
    | SMap --TODO: polymorphic by key?
    | SQueue
    | SPriorityQueue
    deriving (Eq, Show)

{-| Value type. -}
data Type
    = TVoid -- ^ GML 'undefined'
    | TReal | TString
    | TArray Type | TArray2 Type
    | TColor
    | TId Resource -- ^ Resource descriptor
    | TStructure Structure Type -- ^ Data structure descriptor
    | TUnknown [Type] -- ^ Unknown type with possibilities, if any
    deriving (Eq, Show)

instance Semigroup Type where
    TUnknown [] <> t2 = t2
    t1 <> TUnknown [] = t1
    TUnknown t1 <> TUnknown t2 = TUnknown $ t1 ++ t2
    TUnknown t1 <> t2 = TUnknown $ t2 : t1
    t1 <> TUnknown t2 = TUnknown $ t1 : t2
    t1 <> t2 = TUnknown [t1, t2]

instance Monoid Type where
    mempty = TUnknown []

{-| Boolean. In GML, `true` is just any real value which is greater than 0.5.
    Maybe some more typechecking will be added to that later. -}
tBool :: Type
tBool = TReal

{-| Integer. In GML, there is no separate type for integral values.
    Reserved for future typechecking. -}
tInt :: Type
tInt = TReal

{-| Real value between 0 and 1. Reserved for future typechecking. -}
tAlpha :: Type
tAlpha = TReal

{- |Resource descriptors. -}
tInstance, tSprite, tObject, tRoom :: Type
tInstance = TReal
tObject   = TId RObject
tRoom     = TId RRoom
tSprite   = TId RSprite

{- |Data structure descriptors. -}
tGrid, tList, tMap, tPriorityQueue, tQueue, tStack :: Type -> Type
tGrid          = TStructure SGrid
tList          = TStructure SList
tMap           = TStructure SMap
tPriorityQueue = TStructure SPriorityQueue
tQueue         = TStructure SQueue
tStack         = TStructure SStack

{- |Unknown type. -}
tUnknown :: Type
tUnknown = mempty

{- |Combine possibilities of two unknown types. -}
tCombine :: Type -> Type -> Type
tCombine = (<>)

{-| Possibly named function argument. -}
type Argument = (Name, Type)

{-| Function or script signature. -}
data Signature =
    [Argument] -- ^ Argument types
    :-> 
    Type -- ^ Return type
    --TODO: variadic and optional arguments
    deriving (Eq, Show)
