{-|
Module      : Language.GML.Events
Description : GM object events

Events in Game Maker objects.
-}

{-# LANGUAGE LambdaCase #-}

module Language.GML.Events where

import Data.Char (isAlpha, isHexDigit)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)

{-| Key state of keyboard or mouse events. -}
data KeyState = Press | Hold | Release
    deriving (Eq, Ord, Show)

{-| Button code of mouse events. -}
data MouseButton = MNone | MAny | MLeft | MMiddle | MRight
    deriving (Eq, Ord, Show)

instance Enum MouseButton where
    toEnum = undefined
    fromEnum = undefined

{-| Keycode of keyboard events. -}
data KeyCode
    = KNone | KAny
    | KUp | KDown | KLeft | KRight
    | KEsc | KEnter | KTab
    | KSpace | KBackspace
    | KShift | KControl | KAlt -- both left and right
    -- TODO: separate left/alt
    | KInsert | KDelete | KHome | KEnd | KPageUp | KPageDown
    | KPause | KPrintScreen
    | KChar Char
    | KFun Int    -- ^ Functional key
    | KNumpad Int -- ^ Numpad digit
    | KNumAdd | KNumSub | KNumMul | KNumDiv | KNumDot
    deriving (Eq, Ord, Show)

instance Enum KeyCode where
    toEnum = \case
        0  -> KNone
        37 -> KLeft
        39 -> KRight
    fromEnum = \case
        KNone -> 0
        KLeft -> 37
        KRight -> 39

type Guid = String

data Stage = Normal | Begin | End
    deriving (Eq, Ord, Show)

instance Enum Stage where
    toEnum = \case
        0 -> Normal
        1 -> Begin
        2 -> End
    fromEnum = \case
        Normal -> 0
        Begin  -> 1
        End    -> 2

data OtherEvent
    = Outside | Boundary
    | Game !Stage --TODO: just bool?
    | Room !Stage --TODO: just bool?
    | NoMoreLives | NoMoreHealth
    | AnimationEnd | PathEnd
    | CloseButton
    | User !Int -- ^ Custom user event
    deriving (Eq, Ord, Show)

instance Enum OtherEvent where
    toEnum = \case
        n -> User $ n - 10
    fromEnum = \case
        User n -> n + 10

{-| Object event. -}
data Event
    = Create
    | Destroy
    | Cleanup
    | Step !Stage
    | Alarm !Int
    | Draw !Stage | DrawPre | DrawPost --TODO: simplify
    | DrawGui !Stage
    | Collision !Guid
    | Mouse       !KeyState !MouseButton
    | MouseGlobal !KeyState !MouseButton
    | MousePos !Bool -- ^ Entering (True) and leaving (False). TODO: enum
    | MouseWheel !Bool -- ^ Mouse wheel up (True) and leaving (False). TODO: enum
    | Keyboard !KeyState !KeyCode
    | Other !OtherEvent
    --TODO: gesture
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
        "Step"       -> Step $ toEnum code
        "Alarm"      -> Alarm code
        "Collision"  -> Collision arg
        "Draw"       -> Draw $ toEnum code
        "Other"      -> Other $ toEnum code
        "KeyPress"   -> Keyboard Press   keycode
        "Keyboard"   -> Keyboard Hold    keycode
        "KeyRelease" -> Keyboard Release keycode
