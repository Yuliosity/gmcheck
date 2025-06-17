{-|
Module      : Language.GML.Checker
Description : GML typecheck

A rudimentary and conservative typechecker for the GML project codebase.
It tries to derive types of variables and expressions based on their assignment order.
-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.GML.Checker
    ( runChecker
    ) where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.RWS
import Data.Either (isLeft)
import Data.Foldable (asum, for_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Debug.Trace
import Lens.Micro.Platform

import Language.GML.AST
import Language.GML.Project
import Language.GML.Types

import Language.GML.Checker.Errors hiding (fromList)
import Language.GML.Checker.Builtin

type Memory = M.Map Name Type

emptyMem :: Memory
emptyMem = M.empty

fromList :: [(Name, Type)] -> Memory
fromList = M.fromList

type Stack = [Memory]

lookupLocal :: Name -> Stack -> Maybe Type
lookupLocal name frames = asum $ map (M.!? name) frames

{-| Project settings. -}
data Settings = Settings
    { _sBuiltin :: !Builtin
    , _sProject :: !Project
    , _sDisabledErrors :: !ErrorSet
    }

makeLenses ''Settings

{-| Checking context. -}
data Context = Context
    { _cSrc     :: !Source  -- ^ Current source name
    , _cScope   :: ![Name]  -- ^ Current object scope
    , _cLocal   :: !Stack   -- ^ Stack of local variable frames
    --, eGlobals :: Memory -- TODO: globals
    , _cObjects :: !(M.Map Name Memory)
    } deriving (Show)

makeLenses ''Context

emptyContext :: Context
emptyContext = Context
    { _cSrc     = SrcScript ""
    , _cScope   = []
    , _cLocal   = []
    , _cObjects = M.singleton "global" emptyMem
    }

{-| Typechecking monad.
    Reader environment: all of the project and built-in engine data.
    Writer output: errors/warnings report.
    State: all derived data about the codebase at the moment. -}
type Checker = RWS Settings Report Context

withFrame :: Memory -> Checker () -> Checker ()
withFrame mem action = do
    cLocal %= (mem :)
    action
    cLocal %= tail

withScope :: Name -> Checker () -> Checker ()
withScope name action = do
    cScope %= (name :)
    action
    cScope %= tail

setLocal :: Name -> Type -> Checker ()
setLocal k v =
    --TODO: dive the stack
    cLocal %= \(f:fs) -> M.insert k v f : fs

report :: Error -> Checker ()
report = let ?pos = zeroPos in reportPos

reportPos :: (?pos :: Pos) => Error -> Checker ()
reportPos err = do
    noErr <- inSet err <$> view sDisabledErrors
    unless noErr $ do
        src <- use cSrc
        tell $ singleError src $ Located ?pos err

{-| Lookup for a builtin variable. -}
lookupBuiltin :: Name -> Checker (Maybe (Type, Bool))
lookupBuiltin name = do
    Builtin {bGlobalVar, bInstanceVar} <- view sBuiltin
    return $ asum (map (M.!? name) [bGlobalVar, bInstanceVar])

{-| Lookup for a variable in a memory dictionary. -}
lookupMem :: Name -> Memory -> Maybe Type
lookupMem = M.lookup

{-| Lookup for a probably uninitialized variable type. -}
lookup :: Variable -> Checker (Maybe Type)
lookup = \case
    --Local or instance variables
    VVar name -> do
        resources <- pResources <$> view sProject
        builtin   <- lookupBuiltin name
        local <- use cLocal
        scope <- head <$> use cScope
        self <- use (cObjects . at scope) --FIXME: report or insert self
        return $ asum
            [ fst <$> builtin               -- #1: built-in variables/constants
            , M.lookup name resources       -- #2: project resources
            , lookupLocal name local        -- #3: local variables
            , self >>= lookupMem name       -- #4: instance variables
            ]
    --Referenced variable
    VField var@(VVar name) field -> do
        object <- use (cObjects . at name)
        case object of
            Nothing  -> report (EUndefinedVar var) >> return Nothing
            Just mem -> return $ lookupMem field mem

    VField _var _name -> return Nothing--error "Chaining is not yet supported"

    --Indexed cell
    VContainer con var expr -> do
        --1. Check the index
        index <- derive expr
        unless (index `isSubtype` indexType con) $ report $ EBadIndex con index
        --2. Check the container itself
        mty <- lookup var
        case mty of
            --2.1. Uninitialized array
            Nothing -> do
                --TODO: init
                return $ Just TVoid
            --2.2. All is OK
            Just (TContainer rcon res) | con == rcon -> return $ Just res
            --2.3. Wrong container type.
            Just res -> do
                report $ EWrongVarType var res (TContainer con TAny) --FIXME: actual expected array type
                return Nothing

    --2D indexed cell
    VContainer2 con var (e1, e2) -> do
        i1 <- derive e1
        when (i1 /= TInt) $ report $ EBadIndex2 con i1
        i2 <- derive e2
        when (i2 /= TInt) $ report $ EBadIndex2 con i2
        mty <- lookup var
        case mty of
            --2.1. Uninitialized array
            Nothing -> do
                --TODO: init
                return $ Just TVoid
            --2.2. All is OK
            Just (TContainer2 rcon res) | con == rcon -> return $ Just res
            --2.3. Wrong container type.
            Just res -> do
                report $ EWrongVarType var res (TContainer2 con TAny) --FIXME: actual expected array type
                return Nothing

setVar :: Variable -> Type -> Checker ()
setVar var ty = case var of
    VVar name -> setLocal name ty
    VContainer  con (VVar name) _ -> setLocal name $ TContainer  con ty
    VContainer2 con (VVar name) _ -> setLocal name $ TContainer2 con ty
    VField (VVar name) field -> do
        cObjects . at name . non M.empty . at field .= Just ty
    _ -> return () --error "changing non-local variables is not implemented yet"


{-| Lookup for a function signature. -}
lookupFn :: Name -> Checker (Maybe Signature)
lookupFn name = do
    builtinFn <- bFunctions <$> view sBuiltin
    -- TODO: derive and store script types
    return $ M.lookup name builtinFn

isCompOp, isNumOp, isBoolOp :: BinOp -> Bool
isCompOp = (`elem` [Less, LessEq, Eq, NotEq, Greater, GreaterEq])
isNumOp = (`elem` [Add, Sub, Mul, Div])
isBoolOp = (`elem` [And, Or, Xor])

{-| Try to derive a result type of a binary operator application.
    In case of impossible combinations, returns `Left` with the expected result. -}
deriveOp :: BinOp -> Type -> Type -> Either Type Type
deriveOp op t t2 | isCompOp op = if t == t2 then Right TBool else Left TBool
deriveOp Nullish TVoid a     = Right a
deriveOp Nullish a _         = Right a
deriveOp Add TString TString = Right TString
deriveOp Add TString _       = Left  TString
deriveOp Add _       TString = Left  TString
deriveOp Div TInt    TInt    = Right TReal
deriveOp op a b | isNumOp op = case (a, b) of
    (TInt, TInt)   -> Right TInt
    (TReal, TReal) -> Right TReal
    _              -> Left  TReal
deriveOp op a b | isBoolOp op = case (a, b) of
    (TInt, TInt)   -> Right TBool
    _              -> Left TBool
deriveOp _ _ _ = error "deriveOp: unreachable"

checkType descr ty expr = do
    varT <- derive expr
    when (varT /= ty) $ report $ EWrongExprType descr ty varT

checkCond = checkType "conditional" TBool

{-| Deriving the expression type. -}
derive :: Expr -> Checker Type
derive = \case
    EUndefined -> pure TVoid
    EBool _    -> pure TBool
    EPointer   -> pure TPointer
    EVariable (Located pos var) -> do
        let ?pos = pos
        mty <- lookup var
        case mty of
            Nothing -> reportPos (EUndefinedVar var) >> return TVoid
            Just ty -> return ty

    ENumber _ -> return TReal
    EString _ -> return TString

    -- TODO: check self/other/noone
    EInstance _ -> return TInstance

    EArray [] -> return $ TArray TAny
    EArray (e1:es) -> do
        t1 <- derive e1
        forM_ es $ \expr -> do
            ty <- derive expr
            when (t1 /= ty) $ report $ WHeteroArray "literal" t1 ty
            --TODO: array of variants?
        return $ TArray t1

    EStruct fields -> do
        fieldsT <- forM fields $ \(field, expr) -> do
            ty <- derive expr
            return (field, ty)
        return $ TStruct fieldsT

    EUnary op expr -> do
        exprT <- derive expr
        case exprT of
            TReal -> return TReal
            _ -> report (EBadUnary op exprT) >> return exprT

    EBinary op e1 e2 -> do
        e1T <- derive e1
        e2T <- derive e2
        case deriveOp op e1T e2T of
            Right res -> return res
            Left  res  -> do
                report (EBadBinary op e1T e2T)
                return res

    ETernary cond e1 e2 -> do
        checkCond cond
        e1T <- derive e1
        e2T <- derive e2
        if e1T == e2T then
            return e1T
        else do
            report (WTernaryDiff e1T e2T)
            return $ e1T <> e2T

    EFuncall (fn, args) -> deriveCall fn args

    --TODO: check if fn is a constructor
    ENew (fn, args) -> deriveCall fn args

    -- TODO: actually derive
    EFunction (Function args _cons _body) -> do
        let argsT = map (, TAny) args
        let bodyT = TAny
        return $ TFunction argsT bodyT

    where
        deriveCall fn args = do
            argsT <- mapM derive args
            msig <- lookupFn fn
            case msig of
                Just sig@(Signature _ _ res) -> do
                    let minn = minArgs sig; maxn = maxArgs sig; na = length argsT
                    if minn == maxn then
                        when (na /= minn) $ report $ EWrongArgNum fn EQ minn na
                    else do
                        when (na <  minn) $ report $ EWrongArgNum fn GT minn na
                        when (na >  maxn) $ report $ EWrongArgNum fn LT maxn na
                    forM_ (zip argsT $ allArgs sig) $ \(a, (name, b)) ->
                        unless (a `isSubtype` b) $ report $ EWrongArgument fn name b a
                    return res
                Nothing -> do
                    scripts <- pScripts <$> view sProject
                    case scripts M.!? fn of
                        Nothing -> report (EUndefinedFunction fn) >> return TAny
                        Just pr -> do
                            --Push the stack frame with arguments
                            let frame = zipWith (\i t -> ("argument" <> pack (show i), t)) [0..] argsT
                            withFrame (fromList frame) $ run pr
                            return TAny --FIXME: return type
                            --Pop the stack frame

{-| Deriving the script signature. -}
{-
scriptDerive :: Source -> Checker Signature
scriptDerive = go where
    go (stmt:rest) = case stmt of
-}

execAssignModify :: Maybe ModifyOp -> Located Variable -> Expr -> Checker ()
execAssignModify op (Located pos var) expr = do
    let ?pos = pos
    varT <- lookup var
    exprT <- derive expr

    -- Check if this is not the constant
    -- TODO: simplify using MonadFail
    case var of
        VVar name -> do
            builtin <- lookupBuiltin name
            case builtin of
                Just (_, True) -> reportPos (EAssignConst var)
                _ -> return ()
        _ -> return ()

    -- Check if the assigned expression doesn't return a value
    when (exprT == TVoid) $ reportPos (ENoResult var)
    case op of
        -- Assignment: "a = 5"
        Nothing -> do
            case varT of
                Just ty | ty /= TVoid && ty /= exprT ->
                    report $ WChangeType var ty exprT
                --It is a freshly declared instance variable
                _ -> return ()
            src <- use cSrc
            case src of
                SrcObject obj _ -> setVar (qualify obj var) exprT
                -- FIXME: src for scripts
                _ -> setVar var exprT
        -- Modification: "a += 5" etc.
        Just op -> do
            case varT of
                Nothing -> reportPos (EUndefinedVar var)
                -- Assuming that all modifying operators preserve the type, don't check the change
                -- TODO: refactor copy-pasta with binary derive
                Just ty ->
                    when (isLeft $ deriveOp (modifyToBin op) ty exprT) $
                        --FIXME: report assignment operators differently
                        reportPos (EBadModify op ty exprT)

exec :: Stmt -> Checker ()
exec = \case
    SDeclare vexp -> forM_ vexp $ \(VarDecl var mExpr mType) -> do
        exprT <- case mExpr of
            Nothing -> return $ fromMaybe TVoid mType
            Just expr -> do
                exprT <- derive expr
                case mType of
                    Nothing -> return exprT
                    Just ty -> do
                        when (exprT /= ty) $ report $ WChangeType (VVar var) exprT ty
                        return exprT
        setVar (VVar var) exprT

    SAssign var expr -> execAssignModify Nothing var expr

    SModify op var expr -> execAssignModify (Just op) var expr

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
        mapM_ exec false

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

    STry block mcatch mfinally -> do
        run block
        case mcatch of
            Nothing -> return ()
            Just (e, body) -> withFrame (fromList [(e, TException)]) $ run body
        for_ mfinally run

    SThrow expr -> checkType "thrown exception" TException expr

    -- Function declarations should have been preprocessed and added by now

    --TODO: for break/continue/exit/return/throw, check that it's the last statement in a block

    _ -> return ()

run :: Program -> Checker ()
run stmts = withFrame emptyMem $ mapM_ exec stmts

runObject :: (Name, Object) -> Checker ()
runObject (name, Object {oEvents}) = do
    traceM $ "Checking " ++ show name
    forM_ (M.toList oEvents) $ \(event, pr) -> do
        cSrc .= SrcObject name event
        traceM ("-- " ++ show event)
        con <- get
        traceShowM con
        withScope name $ run pr

runProject :: Checker ()
runProject = do
    objects <- pObjects <$> view sProject
    forM_ (M.toList objects) runObject

runChecker :: Builtin -> Project -> ErrorSet -> Report
runChecker builtin project disabledErrors =
    snd $ execRWS runProject settings emptyContext
    where
        settings = Settings builtin project disabledErrors
