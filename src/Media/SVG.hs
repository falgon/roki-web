module Media.SVG (
    optimizeSVGCompiler
) where

import           Hakyll

optimizeSVGCompiler :: [String] -> Compiler (Item String)
optimizeSVGCompiler opts = getResourceString >>=
    withItemBody (unixFilter "node_modules/svgo/bin/svgo" $ ["-i", "-", "-o", "-"] ++ opts)
