module Rules.Src.JavaScript (
    rules
) where

import           Hakyll
import           System.FilePath (joinPath)

import           Media           (compressJsCompiler)

rules :: Rules ()
rules = match jsPath $ do
    route $ gsubRoute "contents/" $ const mempty
    compile compressJsCompiler
    where
        jsPath = fromGlob $ joinPath ["contents", "js", "**"]
