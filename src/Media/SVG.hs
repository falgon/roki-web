module Media.SVG (
    optimizeSVGCompiler
) where

import           Hakyll

optimizeSVGCompiler :: [String] -> Compiler (Item String)
optimizeSVGCompiler opts = getResourceString >>=
    withItemBody (unixFilter "npx" $ ["svgo", "-i", "-", "-o", "-"] ++ opts)
