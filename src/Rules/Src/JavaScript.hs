module Rules.Src.JavaScript (
    rules
  , rulesWith
) where

import           Control.Monad    (forM_, when)
import           Hakyll
import           System.Directory (doesPathExist, removePathForcibly)
import           System.FilePath  ((</>))

import           Config.Program   (hakyllConfig)
import           Media            (compressJsCompiler)
import           Media.TS         (compileTypeScriptCompiler)

rules :: Rules ()
rules = rulesWith hakyllConfig compileTypeScriptCompiler

rulesWith :: Configuration -> Compiler (Item String) -> Rules ()
rulesWith conf typeScriptCompiler = do
    preprocess $ cleanupStaleJsOutputs conf
    viteDependency <- makePatternDependency $ viteDependencyPath .||. viteConfigPath .||. tsconfigPath

    -- Vite/TypeScript設定ファイルと可視化向け型定義は公開しないが、変更時にバンドルを再生成させる
    match viteConfigPath $ compile getResourceBody
    match tsconfigPath $ compile getResourceBody
    match visualizationTypesPath $ compile getResourceBody
    match visualizationBundledSourcePath $ compile getResourceBody

    -- Viteでバンドルするエントリーポイント
    rulesExtraDependencies [viteDependency] $
        match visualizationEntryPath $ do
            route tsRoute
            compile typeScriptCompiler

    -- TypeScriptファイルの処理（公開不要ファイルとViteバンドル対象を除外）
    match (runtimeTsPath .&&. complement visualizationBundledSourcePath .&&. complement visualizationEntryPath) $ do
        route tsRoute
        compile typeScriptCompiler

    -- JavaScriptファイルの処理（手動で作成されたもの）
    match jsPath $ do
        route $ gsubRoute "contents/" $ const mempty
        compile compressJsCompiler
  where
    tsRoute = gsubRoute "contents/ts/" (const "js/") `composeRoutes` setExtension "js"
    tsPath = fromGlob "contents/ts/**.ts" .&&. fromRegex "^contents/ts/.+\\.ts$"
    testTsPath = fromGlob "contents/ts/**" .&&. fromRegex "^contents/ts/(.*/)?__tests__/.*|^contents/ts/.+\\.test\\.ts$"
    declarationTsPath = fromGlob "contents/ts/**.d.ts" .&&. fromRegex "^contents/ts/.+\\.d\\.ts$"
    declarationOnlyTsPath = fromGlob "contents/ts/types/**.ts" .&&. fromRegex "^contents/ts/types/.+\\.ts$"
    runtimeTsPath =
        tsPath
            .&&. complement testTsPath
            .&&. complement declarationTsPath
            .&&. complement declarationOnlyTsPath
    visualizationEntryPath = fromGlob "contents/ts/disney-experience-visualizations.ts"
    visualizationSupportPath =
        fromGlob "contents/ts/disney-hotel-card-navigation.ts"
            .||. fromGlob "contents/ts/disney-tab-manager.ts"
    visualizationsPath = fromGlob "contents/ts/visualizations/**.ts" .&&. fromRegex "^contents/ts/visualizations/.+\\.ts$"
    visualizationBundledSourcePath = visualizationsPath .&&. complement testTsPath
    visualizationTypesPath = fromGlob "contents/ts/types.d.ts"
    viteDependencyPath =
        visualizationSupportPath
            .||. visualizationBundledSourcePath
            .||. visualizationTypesPath
    viteConfigPath = fromGlob "vite.config.production.ts"
    tsconfigPath = fromGlob "tsconfig.json"
    jsPath = fromGlob "contents/js/**" .&&. fromRegex "^contents/js/.+$"
    cleanupStaleJsOutputs conf' = do
        let jsRoot = destinationDirectory conf' </> "js"
            staleOutputs = [
                jsRoot </> "__tests__"
              , jsRoot </> "types.d.js"
              , jsRoot </> "types" </> "disney-experience.js"
              ]
        forM_ staleOutputs $ \path ->
            doesPathExist path >>= flip when (removePathForcibly path)
