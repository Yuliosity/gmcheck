{-|
Module      : Language.GML.Checker
Description : GML typecheck

A rudimentary and conservative typechecker for the GML project codebase.
It tries to derive types of variables and expressions based on their assignment order.
-}

module Language.GML.Checker
    ( runChecker
    ) where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.RWS
import Data.Foldable (asum)
import qualified Data.Map as M
import Debug.Trace

import Language.GML.AST
import Language.GML.Project
import Language.GML.Types

import Language.GML.Checker.Errors
import Language.GML.Checker.Builtin
import Data.Maybe (isJust)

type Memory = M.Map Name Type

emptyMem :: Memory
emptyMem = M.empty

{-| Project settings. -}
data Settings = Settings
    { sBuiltin :: !Builtin
    , sProject :: !Project
    }

{-| Checking context. -}
data Context = Context
    { cSrc     :: !Source
    , cLocal   :: !Memory -- TODO: stack
    --, eGlobals :: Memory -- TODO: globals
    , cObjects :: !(M.Map Name Memory)
    }

emptyContext :: Context
emptyContext = Context
    { cSrc     = SScript ""
    , cLocal   = emptyMem
    , cObjects = M.singleton "global" emptyMem
    }

{-| Typechecking monad.
    Reader environment: all of the project and built-in engine data.
    Writer output: errors/warnings report.
    State: all derived data about the codebase at the moment. -}
type Checker = RWS Settings Report Context

report err = do
    src <- gets cSrc
    tell $ singleError src err

{-| Lookup for a variable in a memory dictionary. -}
lookupMem :: Variable -> Memory -> Checker (Maybe Type)
lookupMem var mem = case var of
    --Local or instance variables
    VVar name -> do
        resources <- asks (pResources . sProject)
        builtin   <- asks (lookupBuiltin name . sBuiltin)
        return $ asum
            [ (\(t, _, _) -> t) <$> builtin      -- Check #1: built-in variables/constants
            , TId <$> M.lookup name resources -- Check #2: project resources
            , M.lookup name mem              -- Check #3: previously derived variables
            ]

    VContainer cty var expr -> do
        index <- derive expr
        when (index /= TInt) $ report $ EBadIndex cty index
        --TODO: init unitialized arrays
        ty <- lookupMem var mem
        case ty of
            --TODO: init unitialized arrays
            Nothing -> return Nothing
            Just (TContainer rty res) | cty == rty -> return $ Just res
            Just res -> do
                report $ EWrongVarType var res (TContainer cty TVoid) --FIXME: actual expected array type
                return Nothing

    VContainer2 cty var (e1, e2) -> do
        i1 <- derive e1
        when (i1 /= TInt) $ report $ EBadIndex2 cty i1
        i2 <- derive e2
        when (i2 /= TInt) $ report $ EBadIndex2 cty i2
        ty <- lookupMem var mem
        case ty of
            --TODO: init unitialized arrays
            Nothing -> return Nothing
            Just (TContainer2 rty res) | cty == rty -> return $ Just res
            Just res -> do
                report $ EWrongVarType var res (TContainer2 cty TVoid) --FIXME: actual expected array type
                return Nothing

    _ -> error $ "Shouldn't get there: " ++ show var

{-| Lookup for a probably uninitialized variable type. -}
lookupMaybe :: Variable -> Checker (Maybe Type)
lookupMaybe = \case
    VField var@(VVar name) field -> do
        objects <- gets cObjects
        case M.lookup name objects of 
            Nothing  -> report (EUndefinedVar var) >> return Nothing
            Just mem -> lookupMem (VVar field) mem
    VField _var _name -> return Nothing--error "Chaining is not yet supported"

    var -> gets cLocal >>= lookupMem var

{-| Lookup for a variable type and report an error if it's undefined. -}
lookup :: Variable -> Checker Type
lookup var = do
    ty <- lookupMaybe var
    case ty of
        Nothing -> report (EUndefinedVar var) >> return TVoid
        Just ty -> return ty

setVar :: Variable -> Type -> Checker ()
setVar var ty = do
    ctx <- get
    case var of
        VVar var -> put $ ctx { cLocal = M.insert var ty (cLocal ctx) } -- TODO: lens
        -- VField name var 
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
            Nothing -> report (EUndefinedFunction fn) >> return TAny
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
        varT <- lookupMaybe var
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
                case varT of
                    Nothing -> return ()
                    Just ty -> when (ty /= exprT) $
                        report $ WChangeType var ty exprT
                setVar var exprT
            AModify op -> do
                case varT of
                    Nothing -> report (EUndefinedVar var)
                    -- Assuming that all modifying operators preserve the type, don't check the change
                    -- TODO: refactor copy-pasta with binary derive
                    Just ty ->
                        when (isJust $ deriveOp (BNum op) ty exprT) $
                            report (EBadBinary (BNum op) ty exprT)
        
    SExpression expr ->
        -- If the expression returns anything, the result is actually lost
        checkType "expression statement" TVoid expr

    SWith expr stmt -> do
        varT <- derive expr
        when (varT /= TInstance) $ report $ EWithInstance varT
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

runObject :: (Name, Object) -> Checker ()
runObject (name, Object {oEvents}) = do
    forM_ (M.toList oEvents) $ \(event, pr) -> do
        modify $ \ctx -> ctx {cSrc = SObject name event}
        trace ("Checking " ++ show event) $ run pr

runProject :: Checker ()
runProject = do
    objects <- asks $ pObjects . sProject
    forM_ (M.toList objects) runObject

runChecker :: Builtin -> Project -> Report
runChecker builtin project = snd $ execRWS runProject settings emptyContext where
    settings = Settings builtin project
