{-|
Module      : Language.GML.Events
Description : GM object events

Events in Game Maker objects.
-}

module Language.GML.Events where

import Data.Char (isAlpha, isHexDigit)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)

{-| Key state of keyboard or mouse events. -}
data KeyState = Press | Hold | Release
    deriving (Eq, Ord, Show)

{-| Button code of mouse events. -}
data MouseButton = MLeft | MMiddle | MRight
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

type Guid = String

{-| Object event. -}
data Event
    = Create
    | Destroy
    | Step
    | Alarm Int
    | Draw
    | Collision Guid
    | User Int -- ^ Custom user event
    | Mouse KeyState MouseButton
    | Key KeyState KeyCode
    deriving (Eq, Ord, Show)

instance Read Event where
    readPrec = lift pEvent

pEvent = do
    event <- munch1 isAlpha
    char '_'
    arg <- munch1 (\c -> isHexDigit c || c == '-')
    let code = read arg
    let keycode = toEnum code
    return $ case event of
        "Create"     -> Create
        "Destroy"    -> Destroy
        "Step"       -> Step
        "Alarm"      -> Alarm code
        "Collision"  -> Collision arg
        "Draw"       -> Draw
        "Other"      -> User (code - 10) --FIXME
        "KeyPress"   -> Key Press   keycode
        "Keyboard"   -> Key Hold    keycode
        "KeyRelease" -> Key Release keycode
