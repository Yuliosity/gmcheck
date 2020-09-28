{-|
Module      : Builtin
Description : Built-in GML functions

Types of built-in instance variables and signatures of library functions.
-}

module Builtin where

import qualified Data.Map.Strict as M

import AST (Name)
import Types

-- * Variables

-- | Dictionary for holding variable types.
type VarDict = M.Map Name Type

mkReal var = (var, TReal)

-- |Instance variables
instanceVar :: VarDict
instanceVar = M.fromList $
    map mkReal
        [ "x", "y", "xstart", "ystart", "xprevious", "yprevious"
        , "speed", "direction", "hspeed", "vspeed", "gravity", "gravity_direction", "friction"
        , "depth", "image_index", "image_angle", "image_xscale", "image_yscale"
        ] ++
    [ ("persistent", tBool)
    , ("solid", tBool)
    , ("visible", tBool)
    , ("alarm", TArray TReal)
    , ("sprite_index", tSprite)
    , ("image_blend", TColor)
    , ("image_alpha", tPercent)
    ]

-- |Instance constants
instanceConst :: VarDict
instanceConst = M.fromList $
    map mkReal
        [ "id", "image_number"
        , "bbox_left", "bbox_right", "bbox_top", "bbox_bottom"
        ] ++
    [ ("object_index", tObject)
    ]

-- |Global variables
globalVar :: VarDict
globalVar = M.fromList $
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

-- TODO: keywords 'self', 'all', 'noone' here?

builtinVar :: VarDict
builtinVar = M.unions
    [ instanceVar, instanceConst
    , globalVar, globalConst
    ]

-- * Functions

-- | Dictionary for holding function signatures.
type FunDict = M.Map Name Signature

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
    , ("real",   [TString] :-> TReal)
    , ("string", [TReal] :-> TString)
    ]

-- |TODO: Array functions
arrayFn = M.empty

-- |Mouse functions
mouseFn = M.fromList
    [ ("mouse_check_button", [TReal] :-> tBool)
    ]

-- |Keyboard functions
keyboardFn = M.fromList
    [ ("keyboard_check", [TReal] :-> tBool)
    ]

-- |Drawing functions
drawFn = M.fromList
    [ ("draw_self", [] :-> TVoid)
    , ("draw_sprite", [tSprite, TReal, TReal, TReal] :-> TVoid)
    , ("draw_sprite_ext", [tSprite, TReal, TReal, TReal, TReal, TReal, TReal, TColor, tPercent] :-> TVoid)
    , ("draw_text",   [TReal, TReal, TString] :-> TVoid)
    , ("draw_circle", [TReal, TReal, TReal, tBool] :-> TVoid)
    , ("draw_set_colour", [TColor] :-> TVoid)
    , ("draw_get_colour", [] :-> TColor)
    , ("make_colour_rgb", [TReal, TReal, TReal] :-> TColor)
    , ("make_colour_hsv", [TReal, TReal, TReal] :-> TColor)
    , ("merge_colour",    [TColor, TColor, tPercent] :-> TColor)
    , ("colour_get_red",   [TColor] :-> TReal)
    , ("colour_get_green", [TColor] :-> TReal)
    , ("colour_get_blue",  [TColor] :-> TReal)
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
    [ ("instance_destroy",  [] :-> TVoid)
    , ("instance_find",     [tObject, TReal] :-> tInstance)
    , ("instance_number",   [tObject] :-> TReal)
    , ("instance_place",    [TReal, TReal, tObject] :-> tInstance)
    , ("instance_position", [TReal, TReal, tObject] :-> tInstance)
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
