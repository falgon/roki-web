module Rules.Src.Style (
    rules
) where

import           Hakyll
import           Hakyll.Web.Sass
import           System.FilePath (joinPath)

import           Config          (contentsRoot)

rules :: Rules ()
rules = do
    match css $ do
        route $ gsubRoute "contents/css/" $ const "style/"
        compile compressCssCompiler

    scssDepend <- makePatternDependency scssDep
    match scssDep $ compile getResourceBody
    rulesExtraDependencies [scssDepend] $
        match scss $ do
            route $
                gsubRoute "contents/scss/" (const "style/") `composeRoutes`
                    setExtension "css"
            compile (fmap compressCss <$> sassCompiler)
    where
        css = fromGlob $ joinPath [contentsRoot, "css", "**"]
        scssDep = fromGlob $ joinPath [contentsRoot, "scss", "*", "**.scss"]
        scss = fromGlob $ joinPath [contentsRoot, "scss", "*.scss"]
