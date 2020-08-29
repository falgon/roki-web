module Media.JS (
    compressJsCompiler
) where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Tuple.Extra           (dupe, first)
import           Hakyll                     (Compiler, Item, getResourceString,
                                             itemBody, itemSetBody)
import           Text.Jasmine               (minify)

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = uncurry itemSetBody . first minifyJS . dupe <$> getResourceString
    where
        minifyJS = C.unpack . minify . C.pack . itemBody
