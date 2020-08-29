module Rules.Src.JavaScript (
    rules
) where

import           Hakyll

import           Config.RegexUtils (intercalateDir)
import           Media             (compressJsCompiler)

rules :: Rules ()
rules = match jsPath $ do
    route $ gsubRoute "contents/" $ const ""
    compile compressJsCompiler
    where
        jsPath = fromGlob $ intercalateDir ["contents", "js", "**"]
