{-# LANGUAGE OverloadedStrings #-}
module Media.TS (compileTypeScriptCompiler) where

import           Control.Monad   ((>=>))
import           Hakyll
import           System.FilePath (takeFileName, (</>))
import           System.IO.Temp  (withSystemTempDirectory)
import           System.Process  (callProcess)

-- | TypeScriptファイルをコンパイルするCompiler
-- disney-experience-visualizations.ts の場合はViteバンドラーを使用し、
-- それ以外のファイルは従来のstdin方式でコンパイルする
compileTypeScriptCompiler :: Compiler (Item String)
compileTypeScriptCompiler = do
    -- リソースのファイルパスを取得
    filePath <- getResourceFilePath
    let fileName = takeFileName filePath

    -- disney-experience-visualizations.ts の場合はViteを使用
    if fileName == "disney-experience-visualizations.ts"
        then compileWithVite
        else getResourceString >>= withItemBody compileTS
  where
    -- 従来のstdin方式でのコンパイル
    compileTS = unixFilter "tools/ts-stdin-compile.sh" []
            >=> unixFilter "npx" ["terser", "--compress", "--mangle", "--format", "comments=false", "--ecma", "6", "--"]

-- | Viteを使用してTypeScriptをバンドル・コンパイルする
compileWithVite :: Compiler (Item String)
compileWithVite = do
    content <- unsafeCompiler $ withSystemTempDirectory "vite-build" $ \tmpDir -> do
        -- Viteでビルド（出力先を一時ディレクトリに指定）
        callProcess "npx" ["vite", "build", "--config", "vite.config.production.ts", "--outDir", tmpDir]

        -- 生成されたJavaScriptファイルを読み込む
        let outputFile = tmpDir </> "disney-experience-visualizations.js"
        readFile outputFile

    -- makeItemを使用してHakyllが正しいIdentifierを設定
    makeItem content
