module Rules.Media (
    rules
) where

import           Hakyll
import           System.FilePath (joinPath)

import           Config          (contentsRoot)
import           Media           (optimizeSVGCompiler)

rules :: Rules ()
rules = do
    match svg $ do
        route $ gsubRoute "contents/" $ const ""
        compile $ optimizeSVGCompiler ["-p", "4"]

    match oth $ do
        route $ gsubRoute "contents/" $ const ""
        compile copyFileCompiler
    where
        svg = fromGlob $ joinPath [contentsRoot, "images", "**", "*.svg"]
        oth = fromGlob $ joinPath [contentsRoot, "images", "**"]
