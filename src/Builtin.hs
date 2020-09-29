{-|
Module      : Builtin
Description : Built-in GML functions

Types of built-in instance variables and signatures of library functions.
-}

module Builtin where

import qualified Data.Map.Strict as M

import System.IO.Unsafe

import AST (Name)
import TypeParser

builtinVar :: VarDict
builtinVar = undefined

builtinFn :: FunDict
builtinFn = undefined