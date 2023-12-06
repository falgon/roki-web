{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field.RokiDiary (
    module Contexts.Field.RokiDiary.GAdsense
  , font
) where

import           Contexts.Field.RokiDiary.GAdsense

import           Lucid.Base                        (Html)
import           Lucid.Html5

-- c.f.
-- https://fonts.google.com/share?selection.family=Noto%2BSans%2BJP%7CUbuntu%2BMono
font :: Html ()
font = link_ [
    href_ "https://fonts.googleapis.com/css2?family=Noto+Sans+JP&family=Ubuntu+Mono&display=swap"
  , rel_ "stylesheet"
  ]
