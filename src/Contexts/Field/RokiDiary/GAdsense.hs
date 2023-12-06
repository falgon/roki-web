{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field.RokiDiary.GAdsense (
    gAdSenseHeader
  , gAdSenseBeforeContentBody
  , gAdSenseFooter
) where

import qualified Data.Text.Lazy as TL
import           Lucid.Base     (Html)
import           Lucid.Html5

gAdSenseBeforeContentBody :: Html ()
gAdSenseBeforeContentBody = do
    script_ [
        async_ mempty
      , src_ "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-5658861742931397"
      , crossorigin_ "anonymous"
      ]
        TL.empty
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
    script_ [
        async_ mempty
      , src_ "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-5658861742931397"
      , crossorigin_ "anonymous"
      ]
        TL.empty
    ins_ [
        class_ "adsbygoogle"
      , style_ "display:block"
      , data_ "ad-format" "autorelaxed"
      , data_ "ad-client" "ca-pub-5658861742931397"
      , data_ "ad-slot" "1547222058"
      ]
        mempty
    script_ "(adsbygoogle = window.adsbygoogle || []).push({});"

gAdSenseHeader :: Html ()
gAdSenseHeader = script_ [
    data_ "ad-client" "ca-pub-5658861742931397"
  , async_ mempty
  , src_ "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
  ]
    TL.empty
