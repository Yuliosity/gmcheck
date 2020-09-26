{-|
Module      : Checker
Description : GML typecheck

A rudimentary and conservative typechecker for the GML project codebase.
It tries to derive types of variables and expressions based on their assignment order.
-}

{-# LANGUAGE LambdaCase #-}

module Checker where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.RWS
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import qualified Data.Map as M

import AST
import Project
import Types
import Errors

type Memory = M.Map VarName Type

data Env = Env
    { eVars    :: M.Map VarName Type
    --, eScope   :: [Memory] -- TODO: stack
    --, eGlobals :: Memory -- TODO: globals
    , eObjects :: M.Map String Memory
    }

emptyEnv :: Env
emptyEnv = Env
    { eVars    = M.empty
    -- , eScope   = []
    , eObjects = M.empty
    }

-- annotate :: Source -> Memory -> 

type Log = [Error]

report err = tell [err]

type Checker = RWS Project Log Env

lookup :: Variable -> Checker Type
lookup var = case var of
    VVar name -> do
        resources <- asks pResources
        vars <- gets eVars
        return $ fromMaybe tUnknown $ (TId <$> M.lookup name resources) <|> M.lookup name vars
        --report a warning if not found?
    VArray name expr -> do
        index <- derive expr
        when (index /= tBool) $ report $ EArrayIndex var index
        ty <- lookup (VVar name)
        case ty of
            TArray res -> return res
            res -> do
                report $ EWrongType var res (TArray TVoid)
                return tUnknown
    _ -> return tUnknown

setVar :: Variable -> Type -> Checker ()
setVar var ty = do
    env <- get
    case var of
        VVar var -> put $ env { eVars = M.insert var ty (eVars env) } -- TODO: lens
        _ -> error "changing non-local variables is not implemented yet"


derive :: Expr -> Checker Type
derive = \case
    ELit (LNumeric _) -> return TReal
    ELit (LString _) -> return TString
    EVar var -> lookup var
    EUnary op expr -> do
        exprT <- derive expr
        case exprT of
            TReal -> return TReal
            _ -> report (EBadUnary op exprT) >> return exprT
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
    SDeclare var Nothing ->
        setVar (VVar var) tUnknown
    SDeclare var (Just expr) -> undefined -- add (derive expr) local

    SAssign var op expr -> do
        varT <- lookup var
        exprT <- derive expr
        case (varT, op, exprT) of
            (TReal, op, TReal) -> return ()
            (TString, AAssign, TString) -> return ()
            (TString, AAdd, TString) -> return ()
            (_, AAssign, _) -> do
                when (varT /= exprT) $
                    report $ WChangeType var varT exprT
                setVar var exprT
                -- change type

    SIf cond true false -> do
        condT <- derive cond
        --when (condT /= tBool) undefined --report error
        run true
        run false
    _ -> return ()
