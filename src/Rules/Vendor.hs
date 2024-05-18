module Rules.Vendor (
    rules
) where

import           Hakyll

import           Control.Monad   (zipWithM_)
import           System.FilePath (joinPath)

vendRule :: Pattern -> FilePath -> Rules ()
vendRule inpat outpath = match inpat $ do
    route $ constRoute outpath
    compile compressCssCompiler

rules :: Bool -> Rules ()
rules isPreview = do
    zipWithM_ vendRule
       [ fontAwesomeSVGPath
       , bulmaPath
       , bulmaToolTipPath
       , highlightPath
       , katexCssPath
       ]
       [ joinPath ["vendor", "fontawesome", "style.css"]
       , joinPath ["vendor", "bulma", "bulma.min.css"]
       , joinPath ["vendor", "bulma", "bulma-tooltip.min.css"]
       , joinPath ["vendor", "highlight", "highlight.css"]
       , joinPath ["vendor", "katex", "katex.min.css"]
       ]

    match (fromGlob $ joinPath ["node_modules", "katex", "dist", "fonts", "**"]) $ do
        route $ gsubRoute "node_modules/katex/dist/" (const "vendor/katex/")
        compile copyFileCompiler

    match (fromGlob $ joinPath ["node_modules", "d3", "dist", "d3.min.js"]) $ do
        route $ gsubRoute "node_modules/d3/dist/" (const "vendor/d3/")
        compile copyFileCompiler

    match (fromGlob $ joinPath ["node_modules", "mathjs", "lib", "browser", "math.js"]) $ do
        route $ gsubRoute "node_modules/mathjs/lib/browser/" (const "vendor/mathjs/")
        compile copyFileCompiler

    if not isPreview then pure () else do
        match (fromGlob $ joinPath ["node_modules", "katex", "dist", "katex.min.js"]) $ do
            route $ gsubRoute "node_modules/katex/dist/" (const "vendor/katex/")
            compile copyFileCompiler

        match (fromGlob $ joinPath ["node_modules", "katex", "dist", "contrib", "auto-render.min.js"]) $ do
            route $ gsubRoute "node_modules/katex/dist/contrib/" (const "vendor/katex/")
            compile copyFileCompiler
    where
        fontAwesomeSVGPath = fromGlob $ joinPath
            ["node_modules", "@fortawesome", "fontawesome-svg-core", "styles.css"]
        bulmaPath = fromGlob $ joinPath
            ["node_modules", "bulma", "css", "bulma.min.css"]
        bulmaToolTipPath = fromGlob $ joinPath
            ["node_modules", "@creativebulma", "bulma-tooltip", "dist", "bulma-tooltip.min.css"]
        katexCssPath = fromGlob $ joinPath
            ["node_modules", "katex", "dist", "katex.min.css"]
        highlightPath = fromGlob $ joinPath
            ["external", "hakyll-css", "css", "tango.css"]

