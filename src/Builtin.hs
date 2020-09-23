module Builtin where

import qualified Data.Map.Strict as M

import AST (VarName, FunName)
import Types

-- $vars
type VarDict = M.Map VarName Type

-- Builtin variables
-- |Instance variables
instanceVars :: VarDict
instanceVars = M.fromList
    [ ("x", TReal), ("y", TReal) -- speed, direction, hspeed, vspeed, gravity :: 
    , ("depth", TReal)
    , ("alarm", TArray TReal)
    ]

-- |Global variables
globalVars :: VarDict
globalVars = M.fromList
    [ ("view_xview", TArray TReal)
    , ("mouse_x", TReal)
    , ("mouse_y", TReal)
    , ("instance_count", TReal)
    ]

-- $functions
type FunDict = M.Map FunName Signature

-- |Math functions
mathFn = M.fromList
    [ ("sin", Sig [TReal] TReal)
    , ("point_distance", Sig (replicate 4 TReal) TReal)
    ]

-- |String functions
stringFn = M.fromList
    [ ("chr", Sig [TReal] TString)
    , ("ord", Sig [TString] TReal)
    , ("string_char_at", Sig [TReal] TString)
    , ("real", Sig [TString] TReal)
    , ("string", Sig [TReal] TString)
    ]

-- |Mouse function
mouseFn = M.fromList
    [ ("mouse_check_button", Sig [TReal] tBool)
    ]

-- |Keyboard function
keyboardFn = M.fromList
    [ ("keyboard_check", Sig [TReal] tBool)
    ]

-- |Drawing functions
drawFn = M.fromList
    [ ("draw_self", Sig [] TVoid)
    , ("draw_sprite", Sig [tSprite, TReal, TReal, TReal] TVoid)
    ]

-- |Instance functions
instanceFn = M.fromList
    [ ("instance_destroy", Sig [] TVoid)
    , ("instance_find", Sig [tObject, TReal] tInstance)
    ]

-- |Events
eventFn = M.fromList
    [ ("event_inherited", Sig [] TVoid)
    ]

builtinFns :: FunDict
builtinFns = M.unions
    [ mathFn, stringFn
    , mouseFn, keyboardFn
    , drawFn
    , instanceFn, eventFn
    ]
