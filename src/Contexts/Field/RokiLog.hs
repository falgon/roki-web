{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field.RokiLog (
    gAdSenseBeforeContentBody
  , footerAdditionalComponent
  , font
) where

import           Contexts.Field.RokiLog.GAdsense
import           Contexts.Field.RokiLog.PowertedBy

import           Lucid.Base                        (Html)
import           Lucid.Html5

footerAdditionalComponent :: Html ()
footerAdditionalComponent = gAdSenseFooter <> haskellJpLogo

-- c.f.
-- https://fonts.google.com/share?selection.family=Noto%2BSerif%2BJP%7CUbuntu%2BMono
font :: Html ()
font = link_ [
    href_ "https://fonts.googleapis.com/css2?family=Noto+Serif+JP&family=Ubuntu+Mono&display=swap"
  , rel_ "stylesheet"
  ]
