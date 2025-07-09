{-# LANGUAGE OverloadedStrings #-}
module Media.TS (compileTypeScriptCompiler) where

import           Control.Monad (void)
import           Hakyll

compileTypeScriptCompiler :: Compiler (Item String)
compileTypeScriptCompiler = do
    item <- getResourceString
    let tsCode = itemBody item

    -- 一時ファイルにTypeScriptコードを書き込み
    let tempTsFile = "/tmp/temp.ts"
    let tempJsFile = "/tmp/temp.js"

    -- TypeScriptをJavaScriptにコンパイル
    void $ unixFilter "sh" ["-c", "echo '" ++ escapeQuotes tsCode ++ "' > " ++ tempTsFile ++ " && npx tsc --target ES5 --module commonjs --lib ES5,DOM --strict --skipLibCheck --noEmitOnError --outDir /tmp --rootDir /tmp " ++ tempTsFile] ""

    -- コンパイルされたJavaScriptを読み込み
    jsCode <- unixFilter "cat" [tempJsFile] ""

    -- JavaScriptをminify
    minifiedJs <- unixFilter "npx" ["terser", "--compress", "--mangle", "--format", "comments=false", "--ecma", "5", "--"] jsCode

    return $ itemSetBody minifiedJs item
  where
    escapeQuotes = concatMap (\c -> if c == '\'' then "'\"'\"'" else [c])
