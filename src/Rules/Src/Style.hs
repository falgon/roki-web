module Rules.Src.Style (
    rules
) where

import           Hakyll
import           Hakyll.Web.Sass

import           Config            (contentsRoot)
import           Config.RegexUtils (intercalateDir)

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
        css = fromGlob $ intercalateDir [contentsRoot, "css", "**"]
        scssDep = fromGlob $ intercalateDir [contentsRoot, "scss", "*", "**.scss"]
        scss = fromGlob $ intercalateDir [contentsRoot, "scss", "*.scss"]
