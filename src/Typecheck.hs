{-# LANGUAGE LambdaCase #-}

module Typecheck where

import Prelude hiding (lookup)

import Control.Monad.Trans.RWS
import Data.Void (Void)
import qualified Data.Map as M

import AST
import Project

data Resource = RSprite | RSound | RObject | RRoom
    deriving (Eq, Show)

data Type
    = TVoid -- ^ Should be used only as a return type
    | TReal | TString | TArray Type | TArray2 Type
    | TId Resource -- ^ Resource descriptor
    | TUnknown [Type] -- ^ Unknown type with possibilities, if any
    deriving (Eq, Show)

tBool, tInstance, tSprite, tObject :: Type
tBool = TReal
tInstance = TReal
tSprite = TId RSprite
tObject = TId RObject
tUnknown = TUnknown []

data Signature = Sig [Type] Type --TODO: variadic and optional arguments
    deriving (Eq, Show)

type Memory = M.Map VarName Type

data Env {- =
    { eVars :: M.Map VarName Type
    , eScope :: [Memory]
    --, eGlobals :: Memory
    , eObjects :: M.Map OName Memory
    }
    -}

-- annotate :: Source -> Memory -> 

data Error
    = WChangeType Variable Type
    | EBadUnary UnOp Type
    | EBadBinary BinOp Type Type

type Log = [Error]

type Checker = RWS Project Log Memory

lookup :: Variable -> Checker Type
lookup = do
    -- check locals
    -- check 
    return undefined

report err = tell [err]

derive :: Expr -> Checker Type
derive = \case
    ELit (LNumeric _) -> return TReal
    ELit (LString _) -> return TString
    EVar var -> undefined --lookup in memory
    EUnary op expr -> do
        exprT <- derive expr
        case exprT of
            TReal -> return TReal
            _ -> report (EBadUnary op exprT) >> return tUnknown
    EBinary op e1 e2 -> do --check for consistency
        e1T <- derive e1
        e2T <- derive e2
        case (op, e1T, e2T) of
            (BAdd, TString, TString) -> return TString
            (_, TReal, TReal) -> return TReal
            _ -> report (EBadBinary op e1T e2T) >> return tUnknown
            --(_, TString, _) -> undefined --report error
            --(_, _, TString) -> undefined --report error
    EFuncall fn args -> undefined --check consistency

run :: Source -> Checker ()
run = mapM_ $ \case
    SDeclare var Nothing -> undefined -- add TUnknown local
    SDeclare var (Just expr) -> undefined -- add (derive expr) local
    SAssign var op expr -> do
        varT <- lookup var
        exprT <- derive expr
        case (varT, op, exprT) of
            (TReal, op, TReal) -> return ()
            (TString, AAssign, TString) -> return ()
            (TString, AAdd, TString) -> return ()
            (_, AAssign, _) -> do
                tell [WChangeType var exprT]
                -- change type
            
    SIf cond true false -> do
        condT <- derive cond
        --when (condT /= tBool) undefined --report error
        run true
        run false
    _ -> pure ()
