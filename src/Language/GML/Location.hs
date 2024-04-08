module Language.GML.Location where

import Language.GML.Events (Event)
import Language.GML.Types (Name)

{-| A code position inside a source. -}
-- data Pos = Pos {row :: !Int, col :: !Int}

{-| A code source. -}
data Source
    -- | Game script
    = SrcScript !Name
    -- | Object event 
    | SrcObject !Name !Event
    -- | Room creation code
    | SrcRoom   !Name 
    deriving (Eq, Ord)

{-| A global code location inside the project. -}
data Loc = Loc {source :: !Source, row :: !Int, col :: !Int}

data Located a = Located {unLoc :: !a, getLoc :: Loc}
