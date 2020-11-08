{-|
Module      : Language.GML.Checker.Report
Description : Built-in GML functions

Types of built-in instance variables and signatures of library functions.
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.GML.Checker.Report where

import Data.ByteString.Lazy as BS
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import Language.GML.Checker.Errors

htmlReport :: Log -> Html
htmlReport log = docTypeHtml $ do
    H.head $ do
        H.title "Report"
    body $ do
        p "All errors"
        ul $ mapM_ (li . toHtml . pretty) log

save :: FilePath -> Html -> IO ()
save path = BS.writeFile path . renderHtml
