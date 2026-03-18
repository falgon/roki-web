module Rules.Src.JavaScript (
    rules
) where

import           Control.Monad    (forM_, when)
import           Hakyll
import           System.Directory (doesPathExist, removePathForcibly)
import           System.FilePath  ((</>))

import           Config.Program   (hakyllConfig)
import           Media            (compressJsCompiler)
import           Media.TS         (compileTypeScriptCompiler)

rules :: Rules ()
rules = do
    preprocess cleanupStaleJsOutputs
    viteDependency <- makePatternDependency $ viteDependencyPath .||. viteConfigPath .||. tsconfigPath

    -- Vite/TypeScript設定ファイルと可視化向け型定義は公開しないが、変更時にバンドルを再生成させる
    match viteConfigPath $ compile getResourceBody
    match tsconfigPath $ compile getResourceBody
    match visualizationTypesPath $ compile getResourceBody

    -- Viteでバンドルするエントリーポイント
    rulesExtraDependencies [viteDependency] $
        match visualizationEntryPath $ do
            route tsRoute
            compile compileTypeScriptCompiler

    -- TypeScriptファイルの処理（公開不要ファイルとViteバンドル対象を除外）
    match (runtimeTsPath .&&. complement visualizationBundledSourcePath .&&. complement visualizationEntryPath) $ do
        route tsRoute
        compile compileTypeScriptCompiler

    -- JavaScriptファイルの処理（手動で作成されたもの）
    match jsPath $ do
        route $ gsubRoute "contents/" $ const mempty
        compile compressJsCompiler
  where
    tsRoute = gsubRoute "contents/ts/" (const "js/") `composeRoutes` setExtension "js"
    tsPath = fromRegex "^contents/ts/.+\\.ts$"
    testTsPath = fromRegex "^contents/ts/(.*/)?__tests__/.*|^contents/ts/.+\\.test\\.ts$"
    declarationTsPath = fromRegex "^contents/ts/.+\\.d\\.ts$"
    declarationOnlyTsPath = fromRegex "^contents/ts/types/.+\\.ts$"
    runtimeTsPath =
        tsPath
            .&&. complement testTsPath
            .&&. complement declarationTsPath
            .&&. complement declarationOnlyTsPath
    visualizationEntryPath = fromRegex "^contents/ts/disney-experience-visualizations\\.ts$"
    visualizationSupportPath = fromRegex "^contents/ts/(disney-hotel-card-navigation|disney-tab-manager)\\.ts$"
    visualizationsPath = fromRegex "^contents/ts/visualizations/.+\\.ts$"
    visualizationBundledSourcePath = visualizationsPath .&&. complement testTsPath
    visualizationTypesPath = fromRegex "^contents/ts/types\\.d\\.ts$"
    viteDependencyPath =
        visualizationSupportPath
            .||. visualizationBundledSourcePath
            .||. visualizationTypesPath
    viteConfigPath = fromRegex "^vite\\.config\\.production\\.ts$"
    tsconfigPath = fromRegex "^tsconfig\\.json$"
    jsPath = fromRegex "^contents/js/.+$"
    cleanupStaleJsOutputs = do
        let jsRoot = destinationDirectory hakyllConfig </> "js"
            staleOutputs = [
                jsRoot </> "__tests__"
              , jsRoot </> "types.d.js"
              , jsRoot </> "types" </> "disney-experience.js"
              ]
        forM_ staleOutputs $ \path ->
            doesPathExist path >>= flip when (removePathForcibly path)
