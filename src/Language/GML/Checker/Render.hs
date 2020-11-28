{-|
Module      : Language.GML.Checker.Render
Description : HTML rendering of errors report
-}

module Language.GML.Checker.Render where

import Control.Monad (forM_)
import Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import Language.GML.Checker.Errors
import Language.GML.AST
import Language.GML.Types
import Language.GML.Events

instance ToMarkup Variable where
    toMarkup = \case
        VVar name -> toMarkup name
        VField name var -> do toMarkup name; "."; toMarkup var
        VContainer  _con name _expr      -> do toMarkup name; "[..]" --TODO: show expr
        VContainer2 _con name (_e1, _e2) -> do toMarkup name; "[..,..]" --TODO: show expr

instance ToMarkup UnOp where
    toMarkup = \case
        UBitNeg  -> "~"
        UNeg     -> "-"
        UNot     -> "!"
        UPreDec  -> "--"
        UPreInc  -> "++"
        UPostDec -> "--"
        UPostInc -> "++"

instance ToMarkup BinOp where
    toMarkup = \case
        BNum Add -> "+"
        BNum Sub -> "-"
        BNum Mul -> "*"
        BNum Div -> "/"
        BNum Mod -> "mod"
        BNum IntDiv -> "div"
        BNum BitAnd -> "&"
        BNum BitOr  -> "|"
        BNum BitXor -> "^"
        BNum Shr    -> ">>"
        BNum Shl    -> "<<"
        BBool And -> "&&"
        BBool Or  -> "||"
        BBool Xor -> "^^"
        BComp Eq        -> "=="
        BComp NotEq     -> "!="
        BComp Less      -> "<"
        BComp LessEq    -> "<="
        BComp Greater   -> ">"
        BComp GreaterEq -> ">="

instance ToMarkup NumOp where
    toMarkup = \case
        Add -> "+="
        Sub -> "-="
        Mul -> "*="
        Div -> "/="
        BitAnd -> "&="
        BitOr  -> "|="
        BitXor -> "^="
        op  -> toMarkup $ show op --impossible

instance ToMarkup Container where
    toMarkup = \case
        SArray -> "array"
        SStack -> "stack"
        SList  -> "list"
        SMap   -> "map"
        SQueue -> "queue"
        SPriorityQueue -> "pqueue"

instance ToMarkup Container2 where
    toMarkup = \case
        SArray2 -> "array2d"
        SGrid   -> "grid"

instance ToMarkup Type where
    toMarkup = \case
        TAny    -> "any"
        TUnknown opt -> do "{"; markupMany opt; "}"
        TVoid   -> "void"
        TReal   -> "real"
        TString -> "string"
        TPtr    -> "ptr"
        TMatrix -> "matrix"
        TNewtype n -> toMarkup n
        TContainer  con ty -> do toMarkup con; "<"; toMarkup ty; ">"
        TContainer2 con ty -> do toMarkup con; "<"; toMarkup ty; ">"
        where
            markupMany [] = mempty
            markupMany [x] = toMarkup x
            markupMany (x:xs) = do toMarkup x; "|"; markupMany xs

instance ToMarkup Event where
    toMarkup = toMarkup . show

instance ToMarkup Error where
    toMarkup = \case
        WChangeType var from to -> do "Type of "; m var; " might be changed from "; m from; " to "; m to
        WHeteroArray var t1 t2 -> do "Array "; m var; " seems to contain "; m t1; " as well as "; m t2
        EUndefinedVar var -> do "Referencing an undefined variable "; m var
        EUndefinedFunction fun -> do "Calling an undefined function "; m fun
        ENoResult fun -> do "Function "; m fun; " doesn't return anything"
        EWrongExprType descr need ty -> do "Type of "; m descr; " should be "; m need; ", but seems to be "; m ty
        EWrongVarType  var   need ty -> do "Type of "; m var;   " should be "; m need; ", but seems to be "; m ty
        EWrongArgNum fun ord n1 n2 -> do "Function "; m fun; " is expected to have "; bound; m n1; " argument(s), but got "; m n2 where
            bound = case ord of
                EQ -> ""
                LT -> "no more than "
                GT -> "at least "
        EBadUnary  op ty    -> do "Operator "; m op; " cannot be applied to "; m ty
        EBadBinary op t1 t2 -> do "Operator "; m op; " cannot be applied to "; m t1; " and "; m t2
        EBadModify op t1 t2 -> do "Operator "; m op; " cannot be applied to "; m t1; " and "; m t2
        EAssignConst var -> do "Cannot assign to a constant "; m var
        EBadIndex  con ty -> do m con; " should be indexed with "; m (indexType  con); ", but got "; m ty
        EBadIndex2 con ty -> do m con; " should be indexed with ints, but got "; m ty
        EWrongArgument fun name need ty -> do "Argument "; m name; " of function "; m fun; " should be "; m need; ", but seems to be "; m ty
        EWithInstance ty -> do "Parameter of the 'with' clause should be an instance, but seems to be "; m ty
        err -> toMarkup $ show err
        where
            m :: ToMarkup a => a -> Markup
            m = toMarkup

instance ToMarkup Source where
    toMarkup = \case
        SScript name -> toMarkup name
        SObject name event -> toMarkup name >> " : " >> toMarkup event

renderLog :: Log -> Html
renderLog = ul . mapM_ (li . toHtml)

htmlReport :: Report -> Html
htmlReport (Report logs) = docTypeHtml $ do
    H.head $ do
        H.title "Report"
    body $ do
        forM_ (M.toList logs) $ \(src, log) -> do
            p $ toHtml src
            renderLog log

save :: FilePath -> Html -> IO ()
save path = BS.writeFile path . renderHtml
