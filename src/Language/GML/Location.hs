module Language.GML.Location where

import Data.Function (on)

import Language.GML.Events (Event)
import Language.GML.Types (Name)

-- | A code position inside a source.
data Pos = Pos !Int !Int
    deriving (Eq, Ord, Show)

zeroPos :: Pos
zeroPos = Pos 0 0

-- | A code source.
data Source
    = -- | Game script
      SrcScript !Name
    | -- | Object event
      SrcObject !Name !Event
    | -- | Room creation code
      SrcRoom !Name
    deriving (Eq, Ord, Show)

-- | A global code location inside the project.
data Location = Location {source :: !Source, row :: !Int, col :: !Int}
    deriving (Eq, Ord, Show)

-- | A code entity with a position (without source).
data Located a = Located {getPos :: !Pos, unLoc :: !a}
    deriving (Show)

instance (Eq a) => Eq (Located a) where
    (==) = (==) `on` unLoc

instance (Ord a) => Ord (Located a) where
    compare = compare `on` unLoc
