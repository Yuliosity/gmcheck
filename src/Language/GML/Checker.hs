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

import Control.Monad (guard, unless, void, when)
import Control.Monad.Trans.RWS
import Data.Either (isLeft, partitionEithers)
import Data.Foldable (asum, for_, traverse_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Traversable (for)
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
    , _sMacros :: M.Map Name Expr
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
        tell $ singleError src $ err :@ ?pos

{-| Lookup for a builtin variable. -}
lookupBuiltin :: Name -> Checker (Maybe (Type, Bool))
lookupBuiltin = \case
    -- TODO: specify the instance type
    "self"  -> pure $ Just (TInstance, True)
    "other" -> pure $ Just (TInstance, True)
    "noone" -> pure $ Just (TInstance, True)
    name -> do
        Builtin {bGlobalVar, bInstanceVar} <- view sBuiltin
        pure $ asum (map (M.!? name) [bGlobalVar, bInstanceVar])

{-| Lookup for a variable in a memory dictionary. -}
lookupMem :: Name -> Memory -> Maybe Type
lookupMem = M.lookup

{-| Lookup for a probably uninitialized variable type. -}
lookup :: Variable -> Checker (Maybe Type)
lookup = \case
    --Local or instance variables
    VVar name -> do
        -- Macros
        macros <- view sMacros
        case M.lookup name macros of
            Just expr -> Just <$> derive expr
            Nothing -> do
                resources <- (.pResources) <$> view sProject
                builtin   <- lookupBuiltin name
                local <- use cLocal
                scope <- head <$> use cScope
                self <- use (cObjects . at scope) --FIXME: report or insert self
                pure $ asum
                    [ fst <$> builtin               -- #1: built-in variables/constants
                    , M.lookup name resources       -- #2: project resources
                    , lookupLocal name local        -- #3: local variables
                    , self >>= lookupMem name       -- #4: instance variables
                    ]
    --Referenced variable
    VField var@(VVar name) field -> do
        object <- use (cObjects . at name)
        case object of
            Nothing  -> report (EUndefinedVar var) >> pure Nothing
            Just mem -> pure $ lookupMem field mem

    VField _var _name -> pure Nothing--error "Chaining is not yet supported"

    --Indexed cell
    VContainer con var expr -> do
        let ?pos = expr.pos
        --1. Check the index
        index <- derive expr
        unless (index `isSubtype` indexType con) $ reportPos $ EBadIndex con index
        --2. Check the container itself
        mty <- lookup var
        case mty of
            --2.1. Uninitialized array
            Nothing -> do
                --TODO: init
                pure $ Just TVoid
            --2.2. All is OK
            Just (TContainer rcon res) | con == rcon -> pure $ Just res
            --2.3. Wrong container type.
            Just res -> do
                reportPos $ EWrongVarType var res (TContainer con TAny) --FIXME: actual expected array type
                pure Nothing

    --2D indexed cell
    VContainer2 con var (e1, e2) -> do
        i1 <- derive e1
        when (i1 /= TInt) $
            let ?pos = e1.pos in reportPos $ EBadIndex2 con i1
        i2 <- derive e2
        when (i2 /= TInt) $
            let ?pos = e2.pos in reportPos $ EBadIndex2 con i2
        mty <- lookup var
        case mty of
            --2.1. Uninitialized array
            Nothing -> do
                --TODO: init
                pure $ Just TVoid
            --2.2. All is OK
            Just (TContainer2 rcon res) | con == rcon -> pure $ Just res
            --2.3. Wrong container type.
            Just res -> do
                report $ EWrongVarType var res (TContainer2 con TAny) --FIXME: actual expected array type
                pure Nothing

setVar :: Variable -> Type -> Checker ()
setVar var ty = case var of
    VVar name -> setLocal name ty
    VContainer  con (VVar name) _ -> setLocal name $ TContainer  con ty
    VContainer2 con (VVar name) _ -> setLocal name $ TContainer2 con ty
    VField (VVar name) field -> do
        cObjects . at name . non M.empty . at field .= Just ty
    _ -> pure () --error "changing non-local variables is not implemented yet"


{-| Lookup for a function signature. -}
lookupFn :: Variable -> Checker (Maybe Signature)
lookupFn fun = do
    case fun of
        VVar name -> do
            builtinFn <- (.bFunctions) <$> view sBuiltin
            -- TODO: derive and store script types
            pure $ M.lookup name builtinFn
        _ -> do
            -- TODO: derive local functions
            pure Nothing

isCompOp, isNumOp, isBoolOp :: BinOp -> Bool
isCompOp = (`elem` [Less, LessEq, Eq, NotEq, Greater, GreaterEq])
isNumOp = (`elem` [Add, Sub, Mul, Div])
isBoolOp = (`elem` [And, Or, Xor])

{-| Try to derive a result type of a unary operator application.
    In case of impossible combinations, returns `Left` with the expected result.
    TODO: int-only versions
-}
deriveUnOp :: UnOp -> Type -> Either Type Type
deriveUnOp UNot TBool = Right TBool
deriveUnOp UPos TReal = Right TReal
deriveUnOp UNeg TReal = Right TReal
deriveUnOp UBitNeg TReal = Right TReal
deriveUnOp UPreInc TReal = Right TReal
deriveUnOp UPreDec TReal = Right TReal
deriveUnOp UPostInc TReal = Right TReal
deriveUnOp UPostDec TReal = Right TReal
deriveUnOp _ _ = Left TReal

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
    let ?pos = expr.pos
    when (varT /= ty) $ reportPos $ EWrongExprType descr ty varT

checkCond = checkType "conditional" TBool

{-| Deriving the expression type. -}
derive :: Expr -> Checker Type
derive (e :@ pos) = let ?pos = pos in case e of
    EUndefined -> pure TVoid
    EBool _    -> pure TBool
    EPointer   -> pure TPointer
    EVariable var -> do
        mty <- lookup var
        case mty of
            Nothing -> reportPos (EUndefinedVar var) >> pure TVoid
            Just ty -> pure ty

    ENumber _ -> pure TReal
    EString _ -> pure TString

    EArray [] -> pure $ TArray TAny
    EArray (e1:es) -> do
        t1 <- derive e1
        for_ es $ \expr -> do
            ty <- derive expr
            when (t1 /= ty) $ report $ WHeteroArray "literal" t1 ty
            --TODO: array of variants?
        pure $ TArray t1

    EStruct fields -> do
        fieldsT <- for fields $ \(field, expr) -> do
            ty <- derive expr
            pure (field, ty)
        pure $ TStruct fieldsT

    EUnary op expr -> do
        e1T <- derive expr
        case deriveUnOp op e1T of
            Right res -> pure res
            Left  res -> do
                report (EBadUnary op e1T)
                pure res

    EBinary op e1 e2 -> do
        e1T <- derive e1
        e2T <- derive e2
        case deriveOp op e1T e2T of
            Right res -> pure res
            Left  res  -> do
                report (EBadBinary op e1T e2T)
                pure res

    ETernary cond e1 e2 -> do
        checkCond cond
        e1T <- derive e1
        e2T <- derive e2
        if e1T == e2T then
            pure e1T
        else do
            report (WTernaryDiff e1T e2T)
            pure $ e1T <> e2T

    EFuncall fn args -> deriveCall fn args

    --TODO: check if fn is a constructor
    ENew fn args -> deriveCall fn args

    -- TODO: actually derive
    EFunction (Function args _cons _body) -> do
        (argsT, optArgsT) <- partitionEithers <$> for args \arg -> do
            t <- deriveVarDecl arg
            pure $ case arg.vdExpr of
                Nothing -> Left (arg.vdName, t)
                Just _ -> Right (arg.vdName, t)
        let bodyT = TAny
        
        pure $ TFunction $ Signature argsT (OptArgs optArgsT) bodyT

    where
        deriveCall fn args = do
            argsT <- traverse derive args
            msig <- lookupFn fn
            case msig of
                Just sig@(Signature _ _ res) -> do
                    let minn = minArgs sig; maxn = maxArgs sig; na = length argsT
                    if minn == maxn then
                        when (na /= minn) $ report $ EWrongArgNum fn EQ minn na
                    else do
                        when (na <  minn) $ report $ EWrongArgNum fn GT minn na
                        when (na >  maxn) $ report $ EWrongArgNum fn LT maxn na
                    for_ (zip argsT $ allArgs sig) $ \(a, (name, b)) ->
                        unless (a `isSubtype` b) $ report $ EWrongArgument fn name b a
                    pure res
                Nothing -> do
                    case fn of
                        VVar name -> do
                            scripts <- (.pScripts) <$> view sProject
                            case scripts M.!? name of
                                Nothing -> report (EUndefinedFunction fn) >> pure TAny
                                Just pr -> do
                                    --Push the stack frame with arguments
                                    let frame = zipWith (\i t -> ("argument" <> pack (show i), t)) [0..] argsT
                                    withFrame (fromList frame) $ run pr
                                    pure TAny --FIXME: pure type
                                    --Pop the stack frame
                        _ -> do
                            -- FIXME
                            pure TAny

{-| Deriving the script signature. -}
{-
scriptDerive :: Source -> Checker Signature
scriptDerive = go where
    go (stmt:rest) = case stmt of
-}

execAssignModify :: Maybe ModifyOp -> VarLoc -> Expr -> Checker ()
execAssignModify op (var :@ p1) expr = do
    let ?pos = p1
    varT <- lookup var
    exprT <- derive expr

    -- Check if this is not the constant
    -- TODO: simplify using MonadFail
    case var of
        VVar name -> do
            builtin <- lookupBuiltin name
            case builtin of
                Just (_, True) -> reportPos (EAssignConst var)
                _ -> pure ()
        _ -> pure ()

    -- Check if the assigned expression doesn't pure a value
    when (exprT == TVoid) $ reportPos (ENoResult var)
    case op of
        -- Assignment: "a = 5"
        Nothing -> do
            case varT of
                Just ty | ty /= TVoid && ty /= exprT ->
                    reportPos $ WChangeType var ty exprT
                -- It is a freshly declared instance variable
                _ -> pure ()
            src <- use cSrc
            case src of
                SrcObject obj _ -> setVar (qualify obj var) exprT
                -- FIXME: src for scripts
                _ -> setVar var exprT
        -- Modification: "a += 5" etc.
        Just op -> do
            case varT of
                Nothing -> reportPos $ EUndefinedVar var
                -- Assuming that all modifying operators preserve the type, don't check the change
                -- TODO: refactor copy-pasta with binary derive
                Just ty ->
                    when (isLeft $ deriveOp (modifyToBin op) ty exprT) $
                        -- FIXME: report assignment operators differently
                        reportPos $ EBadModify op ty exprT

deriveVarDecl :: VarDecl -> Checker Type
deriveVarDecl (VarDecl var mExpr mType) = do
    exprT <- case mExpr of
        Nothing -> pure $ fromMaybe TVoid mType
        Just expr -> do
            exprT <- derive expr
            case mType of
                Nothing -> pure exprT
                Just ty -> do
                    when (exprT /= ty) $ report $ WChangeType (VVar var) exprT ty
                    pure exprT
    setVar (VVar var) exprT
    pure exprT

exec :: Stmt -> Checker ()
exec = \case
    SDeclare vexp -> for_ vexp deriveVarDecl

    -- TODO: process global variables
    SGlobalvar vexp -> void $ deriveVarDecl vexp

    -- TODO: save static variables statically
    SStatic vexp -> void $ deriveVarDecl vexp

    SAssign var expr -> execAssignModify Nothing var expr

    SModify op var expr -> execAssignModify (Just op) var expr

    SExpression expr ->
        -- If the expression returns anything, the result is actually lost
        checkType "expression statement" TVoid expr

    SWith expr stmt -> do
        let ?pos = expr.pos
        varT <- derive expr
        when (varT /= TInstance) $ reportPos $ EWithInstance varT
        -- TODO: switch the context
        exec stmt

    SIf cond true false -> do
        checkCond cond
        exec true
        traverse_ exec false

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
            Nothing -> pure ()
            Just (e, body) -> withFrame (fromList [(e, TException)]) $ run body
        for_ mfinally run

    SThrow expr -> checkType "thrown exception" TException expr

    -- Function declarations should have been preprocessed and added by now

    --TODO: for break/continue/exit/return/throw, check that it's the last statement in a block

    _ -> pure ()

run :: Program -> Checker ()
run stmts = withFrame emptyMem $ traverse_ exec stmts

runObject :: (Name, Object) -> Checker ()
runObject (name, Object {oEvents}) = do
    traceM $ "Checking " ++ show name
    for_ (M.toList oEvents) $ \(event, pr) -> do
        cSrc .= SrcObject name event
        traceM ("-- " ++ show event)
        con <- get
        traceShowM con
        withScope name $ run pr

runProject :: Checker ()
runProject = do
    objects <- (.pObjects) <$> view sProject
    for_ (M.toList objects) runObject

collectMacros :: Name -> Project -> [(Name, Expr)]
collectMacros config Project {pScripts, pObjects} = do
    script <- M.elems pScripts <> concatMap (M.elems . (.oEvents)) (M.elems pObjects)
    -- Assuming that all macro declarations are at the top
    SMacro mConf name expr <- script
    guard $ maybe True (== config) mConf
    pure (name, expr)

runChecker :: Builtin -> Project -> ErrorSet -> Report
runChecker builtin project disabledErrors = do
    let macros = M.fromList $ collectMacros "" project
    let settings = Settings builtin project disabledErrors macros
    snd $ execRWS runProject settings emptyContext
