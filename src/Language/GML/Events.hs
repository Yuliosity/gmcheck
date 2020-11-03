{-|
Module      : Language.GML.Events
Description : GM object events

Events in Game Maker objects.
-}

module Language.GML.Events where

import Data.Char (isAlpha, isDigit)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)

{-| Key state of keyboard events. -}
data KeyState = Press | Hold | Release
    deriving (Eq, Ord, Show)

{-| Keycode of keyboard events. -}
data KeyCode = KUp | KDown | KLeft | KRight | KChar Char
    deriving (Eq, Ord, Show)   

instance Enum KeyCode where
    toEnum x = case x of
        37 -> KLeft
        39 -> KRight
    fromEnum x = case x of
        KLeft -> 37
        KRight -> 39

{-| Object event. -}
data Event
    = Create
    | Step
    | Draw
    | Key KeyState KeyCode
    deriving (Eq, Ord, Show)

instance Read Event where
    readPrec = lift pEvent

pEvent = do
    event <- munch1 isAlpha
    char '_'
    code <- toEnum . read <$> munch1 isDigit
    return $ case event of
        "Create" -> Create
        "Step"   -> Step
        "KeyPress"   -> Key Press code
        "Keyboard"   -> Key Hold code
        "KeyRelease" -> Key Release code
