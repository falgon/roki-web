{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field.RokiDiary.GAdsense (
    gAdSenseBeforeContentBody
  , gAdSenseFooter
) where

import           Lucid.Base  (Html)
import           Lucid.Html5

gAdSenseBeforeContentBody :: Html ()
gAdSenseBeforeContentBody = do
    ins_ [
        class_ "adsbygoogle"
      , style_ "display:block"
      , data_ "ad-client" "ca-pub-5658861742931397"
      , data_ "ad-slot" "9593271026"
      , data_ "ad-format" "auto"
      , data_ "full-width-responsive" "true"
      ]
        mempty
    script_ "(adsbygoogle = window.adsbygoogle || []).push({});"

gAdSenseFooter :: Html ()
gAdSenseFooter = do
    ins_ [
        class_ "adsbygoogle"
      , style_ "display:block"
      , data_ "ad-format" "autorelaxed"
      , data_ "ad-client" "ca-pub-5658861742931397"
      , data_ "ad-slot" "1547222058"
      ]
        mempty
    script_ "(adsbygoogle = window.adsbygoogle || []).push({});"
