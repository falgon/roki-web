{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field.RokiLog (
    gAdSenseBeforeContentBody
  , footerAdditionalComponent
  , font
) where

import           Contexts.Field.RokiLog.GAdsense
import           Contexts.Field.RokiLog.PowertedBy

import qualified Data.Text.Lazy                    as TL
import           Lucid.Base                        (Html)
import           Lucid.Html5

footerAdditionalComponent :: Html ()
footerAdditionalComponent = gAdSenseFooter <> haskellJpLogo

-- c.f. 
-- https://fonts.google.com/share?selection.family=Sawarabi%2BGothic%7CSawarabi%2BMincho
font :: Html ()
font = link_ [
    href_ "https://fonts.googleapis.com/css2?family=Sawarabi+Gothic&family=Sawarabi+Mincho&family=Ubuntu+Mono&display=swap"
  , rel_ "stylesheet"
  ]
