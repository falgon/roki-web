{-# LANGUAGE OverloadedStrings #-}

module Rules.Src.JavaScriptSpec (spec) where

import           Control.Monad        (when)
import           Data.IORef           (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import           Data.Time.Clock      (addUTCTime, getCurrentTime)
import           Hakyll
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       removeFile, setModificationTime)
import           System.FilePath      (takeDirectory, (</>))
import           Test.Hspec
import           TestHelpers          (testCompile, withTestSite)

import qualified Rules.Src.JavaScript as JavaScript

spec :: Spec
spec = describe "Rules.Src.JavaScript" $ do
    it "does not rebuild the Vite entrypoint when dependencies are unchanged" $
        withTestSite $ \_ cfg -> do
            writeJavaScriptFixture cfg
            compileCount <- newIORef (0 :: Int)

            testCompile cfg $ JavaScript.rulesWith cfg $ stubTypeScriptCompiler compileCount
            testCompile cfg $ JavaScript.rulesWith cfg $ stubTypeScriptCompiler compileCount

            readIORef compileCount `shouldReturn` 1

    mapM_ viteDependencyCase
        [ ( "bundled visualization source changes"
          , updateProviderFile "contents/ts/visualizations/base.ts" "export const base = 2"
          )
        , ( "bundled visualization source is added"
          , updateProviderFile "contents/ts/visualizations/added.ts" "export const added = 1"
          )
        , ( "bundled visualization source is deleted"
          , removeProviderFile "contents/ts/visualizations/base.ts"
          )
        , ( "support source changes"
          , updateProviderFile "contents/ts/disney-hotel-card-navigation.ts" "export const nav = 2"
          )
        , ( "visualization types change"
          , updateProviderFile "contents/ts/types.d.ts" "declare const d3: object"
          )
        , ( "Vite config changes"
          , updateProviderFile "vite.config.production.ts" "export default { base: './' }"
          )
        , ( "tsconfig changes"
          , updateProviderFile "tsconfig.json" "{\"compilerOptions\":{}}"
          )
        ]

    it "keeps bundled visualization sources out of the published output" $
        withTestSite $ \_ cfg -> do
            writeJavaScriptFixture cfg
            compileCount <- newIORef (0 :: Int)

            testCompile cfg $ JavaScript.rulesWith cfg $ stubTypeScriptCompiler compileCount

            doesFileExist (destinationDirectory cfg </> "js/disney-experience-visualizations.js")
                `shouldReturn` True
            doesFileExist (destinationDirectory cfg </> "js/components.js")
                `shouldReturn` True
            doesFileExist (destinationDirectory cfg </> "js/__tests__/components.test.js")
                `shouldReturn` False
            doesFileExist (destinationDirectory cfg </> "js/types.d.js")
                `shouldReturn` False
            doesFileExist (destinationDirectory cfg </> "js/types/disney-experience.js")
                `shouldReturn` False
            doesFileExist (destinationDirectory cfg </> "js/visualizations/base.js")
                `shouldReturn` False

stubTypeScriptCompiler :: IORef Int -> Compiler (Item String)
stubTypeScriptCompiler compileCount = do
    underlying <- getUnderlying
    when (toFilePath underlying == "contents/ts/disney-experience-visualizations.ts") $
        unsafeCompiler $ atomicModifyIORef' compileCount $ \count -> (count + 1, ())
    makeItem "compiled"

viteDependencyCase :: (String, Configuration -> IO ()) -> Spec
viteDependencyCase (label, updateDependency) =
    it ("rebuilds the Vite entrypoint when " <> label) $
        withTestSite $ \_ cfg -> do
            writeJavaScriptFixture cfg
            compileCount <- newIORef (0 :: Int)

            testCompile cfg $ JavaScript.rulesWith cfg $ stubTypeScriptCompiler compileCount
            updateDependency cfg
            testCompile cfg $ JavaScript.rulesWith cfg $ stubTypeScriptCompiler compileCount
            testCompile cfg $ JavaScript.rulesWith cfg $ stubTypeScriptCompiler compileCount

            readIORef compileCount `shouldReturn` 2

writeJavaScriptFixture :: Configuration -> IO ()
writeJavaScriptFixture cfg = mapM_ (uncurry $ writeProviderFile cfg)
    [ ("vite.config.production.ts", "export default {}")
    , ("tsconfig.json", "{}")
    , ("contents/ts/types.d.ts", "declare const d3: unknown")
    , ("contents/ts/types/disney-experience.ts", "export interface DisneyExperience {}")
    , ("contents/ts/disney-experience-visualizations.ts", "import './visualizations/base'")
    , ("contents/ts/visualizations/base.ts", "export const base = 1")
    , ("contents/ts/components.ts", "export const component = 1")
    , ("contents/ts/__tests__/components.test.ts", "test('component', () => undefined)")
    , ("contents/ts/disney-hotel-card-navigation.ts", "export const nav = 1")
    ]

writeProviderFile :: Configuration -> FilePath -> String -> IO ()
writeProviderFile cfg path body = do
    let fullPath = providerDirectory cfg </> path
    createDirectoryIfMissing True $ takeDirectory fullPath
    writeFile fullPath body

touchProviderFile :: Configuration -> FilePath -> IO ()
touchProviderFile cfg path = do
    now <- getCurrentTime
    setModificationTime (providerDirectory cfg </> path) $ addUTCTime 60 now

updateProviderFile :: FilePath -> String -> Configuration -> IO ()
updateProviderFile path body cfg = do
    writeProviderFile cfg path body
    touchProviderFile cfg path

removeProviderFile :: FilePath -> Configuration -> IO ()
removeProviderFile path cfg = removeFile $ providerDirectory cfg </> path
