module Rules.Src.JavaScript (
    rules
) where

import           Hakyll
import           System.FilePath (joinPath)

import           Media           (compressJsCompiler)
import           Media.TS        (compileTypeScriptCompiler)

rules :: Rules ()
rules = do
    -- TypeScriptファイルの処理（visualizationsディレクトリを除外）
    -- visualizations/内のファイルはdisney-experience-visualizations.tsにバンドルされるため個別処理不要
    match (tsPath .&&. complement visualizationsPath) $ do
        route $ gsubRoute "contents/ts/" (const "js/") `composeRoutes` setExtension "js"
        compile compileTypeScriptCompiler

    -- JavaScriptファイルの処理（手動で作成されたもの）
    match jsPath $ do
        route $ gsubRoute "contents/" $ const mempty
        compile compressJsCompiler
    where
        tsPath = fromGlob $ joinPath ["contents", "ts", "**"]
        visualizationsPath = fromGlob $ joinPath ["contents", "ts", "visualizations", "**"]
        jsPath = fromGlob $ joinPath ["contents", "js", "**"]
