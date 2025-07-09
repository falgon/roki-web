{-# LANGUAGE OverloadedStrings #-}
module Media.TS (compileTypeScriptCompiler) where

import           Control.Monad ((>=>))
import           Hakyll

compileTypeScriptCompiler :: Compiler (Item String)
compileTypeScriptCompiler = getResourceString >>= withItemBody compileTS
  where
    compileTS = unixFilter "tools/ts-stdin-compile.sh" [] >=> unixFilter "npx" ["terser", "--compress", "--mangle", "--format", "comments=false", "--ecma", "6", "--"]
