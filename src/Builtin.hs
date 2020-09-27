{-|
Module      : Builtin
Description : Built-in GML functions

Types of built-in instance variables and signatures of library functions.
-}

module Builtin where

import qualified Data.Map.Strict as M

import AST (VarName, FunName)
import Types

-- $vars
type VarDict = M.Map VarName Type

mkReal var = (var, TReal)

-- |Instance variables
instanceVars :: VarDict
instanceVars = M.fromList $
    map mkReal ["x", "y", "speed", "direction", "hspeed", "vspeed", "gravity", "depth"] ++
    [ ("alarm", TArray TReal)
    ]

-- |Global variables
globalVars :: VarDict
globalVars = M.fromList $
    [ ("view_xview", TArray TReal)
    , ("view_yview", TArray TReal) 
    ] ++ map mkReal
    [ "room_height", "room_width"
    ]

-- |Global constants
globalConst :: VarDict
globalConst = M.fromList $
    [ ("room", tRoom)
    , ("room_first", tRoom)
    , ("room_last", tRoom)
    ] ++ map mkReal
    [ "instance_count", "keyboard_key"
    , "mouse_button" --FIXME: enum
    , "game_id"
    ]

-- $functions
type FunDict = M.Map FunName Signature

mkRtoR fn = (fn, [TReal] :-> TReal)
mkRRtoR fn = (fn, [TReal, TReal] :-> TReal)

-- |Math functions
mathFn = M.fromList $
    map mkRtoR ["sin", "cos", "tan", "abs", "floor", "ceil", "sqr", "srt"] ++
    map mkRRtoR ["lengthdir_x", "lengthdir_y"] ++
    [ ("point_distance", replicate 4 TReal :-> TReal)
    ]

-- |String functions
stringFn = M.fromList
    [ ("chr", [TReal] :-> TString)
    , ("ord", [TString] :-> TReal)
    , ("string_char_at", [TReal] :-> TString)
    , ("real", [TString] :-> TReal)
    , ("string", [TReal] :-> TString)
    ]

-- |Mouse function
mouseFn = M.fromList
    [ ("mouse_check_button", [TReal] :-> tBool)
    ]

-- |Keyboard function
keyboardFn = M.fromList
    [ ("keyboard_check", [TReal] :-> tBool)
    ]

-- |Drawing functions
drawFn = M.fromList
    [ ("draw_self", [] :-> TVoid)
    , ("draw_sprite", [tSprite, TReal, TReal, TReal] :-> TVoid)
    , ("draw_text", [TReal, TReal, TString] :-> TVoid)
    , ("draw_circle", [TReal, TReal, TReal, tBool] :-> TVoid)
    ]

-- |Collision functions
collisionFn = M.fromList
    [ ("place_free", [TReal, TReal] :-> tBool)
    , ("position_meeting", [TReal, TReal, tObject] :-> tBool) --FIXME: object or instance!
    , ("collision_line", [TReal, TReal, TReal, TReal, tObject, tBool, tBool] :-> tBool)
    ]

-- |Movement functions
movementFn = M.fromList
    [ ("move_contact_solid", [TReal, TReal] :-> TVoid)
    , ("move_contact_all",   [TReal, TReal] :-> TVoid)
    , ("motion_add", [TReal, TReal] :-> TVoid)
    ]

-- |Instance functions
instanceFn = M.fromList
    [ ("instance_destroy", [] :-> TVoid)
    , ("instance_find", [tObject, TReal] :-> tInstance)
    ]

-- |Events
eventFn = M.fromList
    [ ("event_inherited", [] :-> TVoid)
    ]

-- |Room functions
roomFn = M.fromList
    [ ("room_goto", [tRoom] :-> TVoid)
    , ("room_next", [tRoom] :-> tRoom)
    , ("room_goto_next",     [] :-> TVoid)
    , ("room_goto_previous", [] :-> TVoid)
    , ("room_add", [] :-> tRoom)
    , ("room_set_height",     [tRoom, TReal] :-> TVoid)
    , ("room_set_width",      [tRoom, TReal] :-> TVoid)
    , ("room_set_persistent", [tRoom, tBool] :-> TVoid)
    ]

-- |Game functions
gameFn = M.fromList
    [ ("game_save", [TString] :-> TVoid)
    , ("game_load", [TString] :-> TVoid)
    , ("game_end",      [] :-> TVoid)
    , ("game_restart",  [] :-> TVoid)
    ]

builtinFn :: FunDict
builtinFn = M.unions
    [ mathFn, stringFn
    , mouseFn, keyboardFn
    , drawFn
    , instanceFn, eventFn
    ]
