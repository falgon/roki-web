{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field.RokiLog.PowertedBy (
    haskellJpLogo
) where

import           Lucid.Base  (Html)
import           Lucid.Html5

haskellJpLogo :: Html ()
haskellJpLogo =
    a_ [ href_ "https://haskell.jp/blog/posts/links.html#roki.dev/roki.log/" ] $
        img_ [
            width_ "234"
          , class_ "mt-1"
          , src_ "https://haskell.jp/img/supported-by-haskell-jp.svg"
          , alt_"Supported By Haskell-jp."
          ]
