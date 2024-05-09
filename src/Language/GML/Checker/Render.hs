{- |
Module      : Language.GML.Checker.Render
Description : HTML rendering of errors report
-}
module Language.GML.Checker.Render where

import Control.Monad (forM_)
import Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Text.Blaze.Html5 as H

-- import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import Language.GML.AST
import Language.GML.Checker.Errors
import Language.GML.Events
import Language.GML.Types

instance ToMarkup Variable where
    toMarkup = \case
        VVar name -> toMarkup name
        VField name var -> do toMarkup name; "."; toMarkup var
        VContainer _con name _expr -> do toMarkup name; "[..]" -- TODO: show expr
        VContainer2 _con name (_e1, _e2) -> do toMarkup name; "[..,..]" -- TODO: show expr

instance ToMarkup UnOp where
    toMarkup = \case
        UBitNeg -> "~"
        UNeg -> "-"
        UNot -> "!"
        UPreDec -> "--"
        UPreInc -> "++"
        UPostDec -> "--"
        UPostInc -> "++"

instance ToMarkup BinOp where
    toMarkup = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
        IntDiv -> "div"
        BitAnd -> "&"
        BitOr -> "|"
        BitXor -> "^"
        Shr -> ">>"
        Shl -> "<<"
        And -> "&&"
        Or -> "||"
        Xor -> "^^"
        Eq -> "=="
        NotEq -> "!="
        Less -> "<"
        LessEq -> "<="
        Greater -> ">"
        GreaterEq -> ">="
        Nullish -> "??"

instance ToMarkup ModifyOp where
    toMarkup = \case
        MAdd -> "+="
        MSub -> "-="
        MMul -> "*="
        MDiv -> "/="
        MBitAnd -> "&="
        MBitOr -> "|="
        MNullish -> "??="

instance ToMarkup Container where
    toMarkup = \case
        SArray -> "array"
        SStack -> "stack"
        SList -> "list"
        SMap -> "map"
        SQueue -> "queue"
        SPriorityQueue -> "pqueue"

instance ToMarkup Container2 where
    toMarkup = \case
        SArray2 -> "array2d"
        SGrid -> "grid"

instance ToMarkup Type where
    toMarkup = \case
        TPointer -> "pointer"
        TAny -> "any"
        TUnknown opt -> do "{"; markupOpt opt; "}"
        TVoid -> "undefined"
        TBool -> "bool"
        TInt -> "int"
        TReal -> "real"
        TString -> "string"
        TPtr -> "ptr"
        TMatrix -> "matrix"
        TStruct fields -> do
            "{"
            forM_ fields (\(name, ty) -> do toMarkup name; ":"; toMarkup ty)
            "}"
        TFunction args ret -> do
            "("
            forM_ args (\(name, ty) -> do toMarkup name; ":"; toMarkup ty)
            ") -> "
            toMarkup ret
        TNewtype n -> toMarkup n
        TContainer con ty -> do toMarkup con; "<"; toMarkup ty; ">"
        TContainer2 con ty -> do toMarkup con; "<"; toMarkup ty; ">"
        TTypeVar n -> toMarkup n
      where
        markupOpt [] = mempty
        markupOpt [x] = toMarkup x
        markupOpt (x : xs) = do toMarkup x; "|"; markupOpt xs

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
        EWrongVarType var need ty -> do "Type of "; m var; " should be "; m need; ", but seems to be "; m ty
        EWrongArgNum fun ord n1 n2 -> do "Function "; m fun; " is expected to have "; bound; m n1; " argument(s), but got "; m n2
          where
            bound = case ord of
                EQ -> ""
                LT -> "no more than "
                GT -> "at least "
        EBadUnary op ty -> do "Operator "; m op; " cannot be applied to "; m ty
        EBadBinary op t1 t2 -> do "Operator "; m op; " cannot be applied to "; m t1; " and "; m t2
        EBadModify op t1 t2 -> do "Operator "; m op; " cannot be applied to "; m t1; " and "; m t2
        EAssignConst var -> do "Cannot assign to a constant "; m var
        EBadIndex con ty -> do m con; " should be indexed with "; m (indexType con); ", but got "; m ty
        EBadIndex2 con ty -> do m con; " should be indexed with ints, but got "; m ty
        EWrongArgument fun name need ty -> do "Argument "; m name; " of function "; m fun; " should be "; m need; ", but seems to be "; m ty
        EWithInstance ty -> do "Parameter of the 'with' clause should be an instance, but seems to be "; m ty
        err -> toMarkup $ show err
      where
        m :: (ToMarkup a) => a -> Markup
        m = toMarkup

instance ToMarkup Source where
    toMarkup = \case
        SrcScript name -> toMarkup name
        SrcObject name event -> toMarkup event >> " of " >> toMarkup name
        SrcRoom name -> "Creation code of" >> toMarkup name

instance ToMarkup Pos where
    toMarkup (Pos 0 0) = "<unknown pos>"
    toMarkup (Pos line col) = toMarkup line >> ":" >> toMarkup col

renderLog :: Log -> Html
renderLog log = ul $ mapM_ renderError log
  where
    renderError (Located _pos err) = li $ toMarkup _pos >> " : " >> toMarkup err

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
