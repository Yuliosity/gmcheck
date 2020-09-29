{-|
Module      : Checker
Description : GML typecheck

A rudimentary and conservative typechecker for the GML project codebase.
It tries to derive types of variables and expressions based on their assignment order.
-}

{-# LANGUAGE LambdaCase #-}

module Checker where

import Prelude hiding (lookup)

import Control.Applicative (Alternative, (<|>))
import Control.Monad
import Control.Monad.Trans.RWS
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import qualified Data.Map as M

import AST
import Builtin
import Project
import Types
import Errors

type Memory = M.Map Name Type

data Env = Env
    { eVars    :: M.Map Name Type
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

{-| Typechecking monad.
    Reader environment: all of the project data.
    Writer output: errors/warnings log.
    State: all derived data about the codebase at the moment. -}
type Checker = RWS Project Log Env

choice :: (Foldable f, Alternative a) => f (a t) -> (a t)
choice = foldl1 (<|>)

{-| Lookup for a variable type. -}
lookup :: Variable -> Checker Type
lookup var = case var of
    VVar name -> do
        resources <- asks pResources
        vars <- gets eVars
        return $ fromMaybe tUnknown $ choice
            [ M.lookup name builtinVar
            , TId <$> M.lookup name resources
            , M.lookup name vars
            ]
        --report a warning if not found?
    {-
    VField name var -> do
        objects <- asks pObjects
        M.lookup name objects
    -}
    VArray name expr -> do
        index <- derive expr
        when (index /= tBool) $ report $ EArrayIndex var index
        ty <- lookup (VVar name)
        case ty of
            TArray res -> return res
            res -> do
                report $ EWrongVarType var res (TArray TVoid)
                return tUnknown
    _ -> return tUnknown

setVar :: Variable -> Type -> Checker ()
setVar var ty = do
    env <- get
    case var of
        VVar var -> put $ env { eVars = M.insert var ty (eVars env) } -- TODO: lens
        _ -> error "changing non-local variables is not implemented yet"

{-| Lookup for a function signature. -}
lookupFn :: FunName -> Checker (Maybe Signature)
lookupFn name = do
    -- TODO: derive and store script types
    return $ M.lookup name builtinFn

binCompat :: BinOp -> Type -> Type -> Bool
binCompat (BNum Add) TString TString = True
binCompat _ TReal TReal = True
binCompat _ _ _ = False

checkType descr ty expr = do
    varT <- derive expr
    when (varT /= ty) $ report $ EWrongExprType descr ty varT

checkCond = checkType "conditional" tBool

{-| Deriving the expression type. -}
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

    EBinary op e1 e2 -> do
        e1T <- derive e1
        e2T <- derive e2

        if binCompat op e1T e2T then
            return e2T
        else do
            report (EBadBinary op e1T e2T)
            return $ e1T `tCombine` e2T

    ETernary cond e1 e2 -> do
        checkCond cond
        e1T <- derive e1
        e2T <- derive e2
        if e1T == e2T then
            return e1T
        else do
            report (WTernaryDiff e1T e2T)
            return $ e1T `tCombine` e2T

    EFuncall fn args -> do
        sig <- lookupFn fn
        case sig of
            Nothing -> report (EUnknownFunction fn) >> return tUnknown
            Just (needed :-> res) -> do
                argsT <- mapM derive args
                --FIXME: check the arguments number
                forM_ (zip needed argsT) $ \((name, a), b) ->
                    when (a /= b) $ report (EWrongArgument fn name a b)
                return res

{-| Deriving the script signature. -}
{-
scriptDerive :: Source -> Checker Signature
scriptDerive = go where
    go (stmt:rest) = case stmt of
-}

run :: Source -> Checker ()
run = mapM_ $ \case
    SDeclare var mExpr -> do
        exprT <- case mExpr of
            Nothing -> return tUnknown -- tEmpty?
            Just expr -> derive expr
        setVar (VVar var) exprT

    SAssign var ass expr -> do
        varT <- lookup var
        exprT <- derive expr

        when (exprT == TVoid) $ report (ENoResult var)
        case ass of
            AAssign -> do
                when (varT /= tUnknown && varT /= exprT) $
                    report $ WChangeType var varT exprT
                setVar var exprT
            AModify op -> do
                -- TODO: refactor copy-pasta with binary derive
                when (not $ binCompat (BNum op) varT exprT) $
                    report (EBadBinary (BNum op) varT exprT)

    SExpression expr ->
        -- If the expression returns anything, the result is actually lost
        checkType "expression statement" TVoid expr

    SIf cond true false -> do
        checkCond cond
        run true
        run false

    SWhile cond block -> do
        checkCond cond
        run block

    SDoUntil block cond -> do 
        run block
        checkCond cond

    SRepeat count block -> do
        checkType "count" TReal count
        run block

    --TODO: for break/continue/exit/return, check that it's the last statement in a block

    _ -> return ()
