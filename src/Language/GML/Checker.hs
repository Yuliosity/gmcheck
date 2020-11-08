{-# LANGUAGE NamedFieldPuns #-}
{-|
Module      : Language.GML.Checker
Description : GML typecheck

A rudimentary and conservative typechecker for the GML project codebase.
It tries to derive types of variables and expressions based on their assignment order.
-}

{-# LANGUAGE LambdaCase #-}

module Language.GML.Checker
    ( runChecker
    ) where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.RWS
import Data.Foldable (asum)
import Data.Maybe (catMaybes, isJust, fromMaybe)
import qualified Data.Map as M

import Language.GML.AST
import Language.GML.Project
import Language.GML.Types

import Language.GML.Checker.Errors
import Language.GML.Checker.Builtin
import Language.GML.Events
import Debug.Trace

type Memory = M.Map Name Type

data Settings = Settings
    { sBuiltin :: !Builtin
    , sProject :: !Project
    }

data Env = Env
    { eVars    :: !VarDict
    --, eScope   :: [Memory] -- TODO: stack
    --, eGlobals :: Memory -- TODO: globals
    , eObjects :: !(M.Map Name Memory)
    } deriving Show

emptyEnv :: Env
emptyEnv = Env
    { eVars    = M.empty
    -- , eScope   = []
    , eObjects = M.empty
    }

report err = tell [err]

{-| Typechecking monad.
    Reader environment: all of the project and built-in engine data.
    Writer output: errors/warnings log.
    State: all derived data about the codebase at the moment. -}
type Checker = RWS Settings Log Env

{-| Lookup for a variable type. -}
lookup :: Variable -> Checker Type
lookup var = case var of
    VVar name -> do
        -- Look for resources
        resources <- asks (pResources . sProject)
        bVar <- asks (lookupBuiltin name . sBuiltin)
        vars <- gets eVars
        return $ fromMaybe TAny $ asum
            [ (\(t, _, _) -> t) <$> bVar      -- Check #1: built-in variables/constants
            , TId <$> M.lookup name resources -- Check #2: project resources
            , M.lookup name vars              -- Check #3: previously derived variables
            ]
        --TODO: report a warning if not found
    {-
    VField name var -> do
        objects <- asks pObjects
        M.lookup name objects
    -}
    -- TODO: check the same for other containers
    VArray name expr -> do
        index <- derive expr
        when (index /= TInt) $ report $ EArrayIndex var index
        ty <- lookup (VVar name)
        case ty of
            TArray res -> return res
            res -> do
                report $ EWrongVarType var res (TArray TVoid)
                return TAny
    _ -> return TAny

setVar :: Variable -> Type -> Checker ()
setVar var ty = do
    env <- get
    case var of
        VVar var -> put $ env { eVars = M.insert var ty (eVars env) } -- TODO: lens
        _ -> return () --error "changing non-local variables is not implemented yet"

{-| Lookup for a function signature. -}
lookupFn :: FunName -> Checker (Maybe Signature)
lookupFn name = do
    builtinFn <- asks (bFunctions . sBuiltin)
    -- TODO: derive and store script types
    return $ M.lookup name builtinFn

deriveOp :: BinOp -> Type -> Type -> Maybe Type
deriveOp (BComp _) t1 t2 | t1 == t2 = Just TBool
deriveOp (BNum Add) TString TString = Just TString
deriveOp (BNum Div) TInt    TInt    = Just TReal
deriveOp _          TInt    TInt    = Just TInt
deriveOp (BNum _)   TReal   TReal   = Just TReal
deriveOp _          _       _       = Nothing

checkType descr ty expr = do
    varT <- derive expr
    when (varT /= ty) $ report $ EWrongExprType descr ty varT

checkCond = checkType "conditional" TBool

{-| Deriving the expression type. -}
derive :: Expr -> Checker Type
derive = \case
    ELiteral (LNumeric _) -> return TReal
    ELiteral (LString  _) -> return TString

    EVariable var -> lookup var

    EUnary op expr -> do
        exprT <- derive expr
        case exprT of
            TReal -> return TReal
            _ -> report (EBadUnary op exprT) >> return exprT

    EBinary op e1 e2 -> do
        e1T <- derive e1
        e2T <- derive e2

        case deriveOp op e1T e2T of
            Just res -> return res
            Nothing  -> do
                report (EBadBinary op e1T e2T)
                return $ e1T <> e2T --FIXME: what should be here?

    ETernary cond e1 e2 -> do
        checkCond cond
        e1T <- derive e1
        e2T <- derive e2
        if e1T == e2T then
            return e1T
        else do
            report (WTernaryDiff e1T e2T)
            return $ e1T <> e2T

    EFuncall fn args -> do
        sig <- lookupFn fn
        case sig of
            Nothing -> report (EUnknownFunction fn) >> return TAny
            Just (needed :-> res) -> do
                argsT <- mapM derive args
                let nn = length needed; na = length argsT
                when (nn /= na) $ report $ EWrongArgNum fn nn na
                --FIXME: check the arguments number
                forM_ (zip needed argsT) $ \((name, a), b) ->
                    when (a /= b) $ report $ EWrongArgument fn name a b
                return res

{-| Deriving the script signature. -}
{-
scriptDerive :: Source -> Checker Signature
scriptDerive = go where
    go (stmt:rest) = case stmt of
-}

exec :: Stmt -> Checker ()
exec = \case
    SDeclare vexp -> forM_ vexp $ \(var, mExpr) -> do
        exprT <- case mExpr of
            Nothing -> return TVoid
            Just expr -> derive expr
        setVar (VVar var) exprT

    SAssign var ass expr -> do
        varT <- lookup var
        exprT <- derive expr

        -- Check if this is not the constant
        -- TODO: simplify using MonadFail
        case var of
            VVar name -> do
                builtin <- asks sBuiltin
                case lookupBuiltin name builtin of
                    Just (_, _, True) -> report (EAssignConst var)
                    _ -> return ()
            _ -> return ()

        -- Check if the assigned expression doesn't return a value
        when (exprT == TVoid) $ report (ENoResult var)
        case ass of
            AAssign -> do
                when (varT /= TAny && varT /= exprT) $
                    report $ WChangeType var varT exprT
                setVar var exprT
            AModify op -> do
                -- TODO: refactor copy-pasta with binary derive
                when (isJust $ deriveOp (BNum op) varT exprT) $
                    report (EBadBinary (BNum op) varT exprT)
        -- Check if the assigned expression doesn't return a value
        
    SExpression expr ->
        -- If the expression returns anything, the result is actually lost
        checkType "expression statement" TVoid expr

    SWith expr stmt -> do
        varT <- derive expr
        when (varT /= TInstance) $ report EWithInstance
        -- TODO: switch the context
        exec stmt

    SIf cond true false -> do
        checkCond cond
        exec true
        case false of
            Nothing -> return ()
            Just stmt -> exec stmt

    SWhile cond stmt -> do
        checkCond cond
        exec stmt

    SDoUntil stmt cond -> do 
        exec stmt
        checkCond cond

    SRepeat count stmt -> do
        checkType "count" TReal count
        exec stmt

    SFor init cond stmt body -> do
        exec init
        checkCond cond
        exec stmt
        exec body

    SBlock stmts -> run stmts

    --TODO: for break/continue/exit/return, check that it's the last statement in a block

    _ -> return ()

run :: Program -> Checker ()
run = mapM_ exec

runObject :: Object -> Checker ()
runObject (Object {oEvents}) = do
    forM_ (M.toList oEvents) $ \(event, pr) ->
        trace ("Checking " ++ show event) $ run pr

runProject :: Checker ()
runProject = do
    objects <- asks $ pObjects . sProject
    forM_ objects runObject

runChecker :: Builtin -> Project -> Log
runChecker builtin project = snd $ execRWS runProject settings emptyEnv where
    settings = Settings builtin project
