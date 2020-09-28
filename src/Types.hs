{-|
Module      : Types
Description : GML Types

Type system of GML values.
-}

module Types where

import Data.Monoid

{-| Resource type. In GML it's actually just a number, but here we want to differ. -}
data Resource
    = RSprite
    | RBackground
    | RSound
    | RObject
    | RRoom
    -- TODO: paths, etc.
    deriving (Eq, Show)

{-| Value type. -}
data Type
    = TVoid -- ^ Should be used only as a return type
    | TReal | TString
    | TArray Type | TArray2 Type
    | TId Resource -- ^ Resource descriptor
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

tBool, tInstance, tSprite, tObject, tRoom, tUnknown :: Type
tBool = TReal
tInstance = TReal
tSprite = TId RSprite
tObject = TId RObject
tRoom = TId RRoom
tUnknown = mempty

tCombine :: Type -> Type -> Type
tCombine = (<>)

{-| Function or script signature. -}
data Signature = [Type] :-> Type --TODO: variadic and optional arguments
    deriving (Eq, Show)
