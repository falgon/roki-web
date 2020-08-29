module Rules.Media (
    rules
) where

import           Hakyll

import           Config            (contentsRoot)
import           Config.RegexUtils (intercalateDir)
import           Media             (optimizeSVGCompiler)

rules :: Rules ()
rules = do
    match svg $ do
        route $ gsubRoute "contents/" $ const ""
        compile $ optimizeSVGCompiler ["-p", "4"]

    match oth $ do
        route $ gsubRoute "contents/" $ const ""
        compile copyFileCompiler
    where
        svg = fromGlob $ intercalateDir [contentsRoot, "images", "**", "*.svg"]
        oth = fromGlob $ intercalateDir [contentsRoot, "images", "**"]
