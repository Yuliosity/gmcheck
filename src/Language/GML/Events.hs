{-|
Module      : Language.GML.Events
Description : GM object events

Events in Game Maker objects.
-}

{-# LANGUAGE StrictData #-}

module Language.GML.Events
    ( Key (..), KeyState (..), MouseButton (..)
    , Guid, Stage (..)
    , AsyncEvent (..), OtherEvent (..), GestureEvent (..), Event (..)
    ) where

import Data.Char (isAlpha, isHexDigit)
import Data.Text (Text, pack)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Web.KeyCode

{-| Key state of keyboard or mouse events. -}
data KeyState = Press | Hold | Release
    deriving (Eq, Ord, Show)

{-| Button code of mouse events. -}
data MouseButton = MNone | MAny | MLeft | MMiddle | MRight
    deriving (Eq, Ord, Show)

instance Enum MouseButton where
    toEnum = undefined
    fromEnum = undefined

type Guid = Text

data Stage = SBegin | SEnd
    deriving (Eq, Ord, Show)

data AsyncEvent
    = AudioPlayback Stage
    | AudioRecording
    | Cloud
    | Dialog
    | Http
    | InAppPurchase
    | ImageLoaded
    | Networking
    | PushNotification
    | SaveLoad
    | Social
    | Steam
    | System
    deriving (Eq, Ord, Show)

data OtherEvent
    = Outside          -- ^ Outside room
    | Boundary         -- ^ Intersect boundary
    | OutsideView  Int -- ^ Outside view
    | BoundaryView Int -- ^ Intersect iew boundary
    | GameEvent Stage
    | RoomEvent Stage
    | NoMoreLives | NoMoreHealth
    | AnimationEnd | PathEnd
    | CloseButton
    | Broadcast
    | User Int         -- ^ Custom user event
    | Async AsyncEvent -- ^ Asynchronous event
    | UnknownEvent Int -- ^ Unknown event
    deriving (Eq, Ord, Show)

instance Enum OtherEvent where
    toEnum = \case
        0  -> Outside
        1  -> Boundary
        2  -> GameEvent SBegin
        3  -> GameEvent SEnd
        4  -> RoomEvent SBegin
        5  -> RoomEvent SEnd
        6  -> NoMoreLives
        7  -> AnimationEnd
        8  -> PathEnd
        9  -> NoMoreHealth
        30 -> CloseButton
        n  | n <= 25  -> User         $ n - 10
        n  | n <= 47  -> OutsideView  $ n - 40
        n  | n <= 57  -> BoundaryView $ n - 50
        60 -> Async ImageLoaded
        62 -> Async Http
        63 -> Async Dialog
        66 -> Async InAppPurchase
        67 -> Async Cloud
        68 -> Async Networking
        69 -> Async Steam
        70 -> Async Social
        71 -> Async PushNotification
        72 -> Async SaveLoad
        73 -> Async AudioRecording
        74 -> Async $ AudioPlayback SBegin
        75 -> Async System
        80 -> Async $ AudioPlayback SEnd
        n  -> UnknownEvent n
    fromEnum = undefined

data GestureEvent
    = Tap
    | DoubleTap
    | DragStart
    | Dragging
    | DragEnd
    | Flick
    | PinchStart
    | PinchIn
    | PinchOut
    | PinchEnd
    | RotateStart
    | Rotating
    | RotateEnd
    | GlobalTap
    | GlobalDoubleTap
    | GlobalDragStart
    | GlobalDragging
    | GlobalDragEnd
    | GlobalFlick
    | GlobalPinchStart
    | GlobalPinchIn
    | GlobalPinchOut
    | GlobalPinchEnd
    | GlobalRotateStart
    | GlobalRotating
    | GlobalRotateEnd
    deriving (Eq, Ord, Show, Enum)

{-| Object event. -}
data Event
    = Create
    | Destroy
    | Cleanup
    | Step | StepExt Stage
    | Alarm Int
    | Draw | DrawExt Stage
    | DrawPre | DrawPost
    | DrawResize
    | DrawGui | DrawGuiExt Stage
    | Collision Guid
    | NoMouse
    | Mouse       KeyState MouseButton
    | MouseGlobal KeyState MouseButton
    | MouseEnter Stage -- ^ Entering and leaving
    | MouseWheelUp | MouseWheelDown
    | NoKeyboard
    | Keyboard KeyState KeyCode
    | Gesture GestureEvent
    | Other OtherEvent
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
        "Cleanup"    -> Cleanup
        "Step"       -> case code of
            0  -> Step
            1  -> StepExt SBegin
            2  -> StepExt SEnd
            n  -> error $ "Unknown Step subid: " ++ show n
        "Alarm"      -> Alarm code
        "Collision"  -> Collision $ pack arg
        "Draw"       -> case code of
            0  -> Draw
            72 -> DrawExt SBegin
            73 -> DrawExt SEnd
            64 -> DrawGui
            74 -> DrawGuiExt SBegin
            75 -> DrawGuiExt SEnd
            65 -> DrawResize
            76 -> DrawPre
            77 -> DrawPost
            n  -> error $ "Unknown Draw subid: " ++ show n
        "Other"      -> case code of
            76 -> Other Broadcast
            _  -> Other $ toEnum code
        "KeyPress"   -> Keyboard Press   keycode
        "Keyboard"   -> Keyboard Hold    keycode
        "KeyRelease" -> Keyboard Release keycode
        "Mouse"      -> case code of
            3  -> NoMouse
            0  -> Mouse       Hold    MLeft
            1  -> Mouse       Hold    MRight
            2  -> Mouse       Hold    MMiddle
            4  -> Mouse       Press   MLeft
            5  -> Mouse       Press   MRight
            6  -> Mouse       Press   MMiddle
            7  -> Mouse       Release MLeft
            8  -> Mouse       Release MRight
            9  -> Mouse       Release MMiddle            
            10 -> MouseEnter  SBegin
            11 -> MouseEnter  SEnd
            50 -> MouseGlobal Hold    MLeft
            51 -> MouseGlobal Hold    MRight
            52 -> MouseGlobal Hold    MMiddle
            53 -> MouseGlobal Press   MLeft
            54 -> MouseGlobal Press   MRight
            55 -> MouseGlobal Press   MMiddle
            56 -> MouseGlobal Release MLeft
            57 -> MouseGlobal Release MRight
            58 -> MouseGlobal Release MMiddle
            60 -> MouseWheelUp
            61 -> MouseWheelDown
            n  -> error $ "Unknown Mouse subid: " ++ show n
        "Gesture"    -> case code of
            64 -> Gesture GlobalTap
            65 -> Gesture GlobalDoubleTap
            66 -> Gesture GlobalDragStart
            67 -> Gesture GlobalDragging
            68 -> Gesture GlobalDragEnd
            69 -> Gesture GlobalFlick
            70 -> Gesture GlobalPinchStart
            71 -> Gesture GlobalPinchIn
            72 -> Gesture GlobalPinchOut
            73 -> Gesture GlobalPinchEnd
            74 -> Gesture GlobalRotateStart
            75 -> Gesture GlobalRotating
            76 -> Gesture GlobalRotateEnd
            _  -> Gesture $ toEnum code
        s -> error $ "Unknown event name: " ++ s
