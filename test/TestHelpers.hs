module TestHelpers (
    withTestSite
  , testCompile
  , cleanupTestSite
) where

import           Control.Exception (bracket)
import           Hakyll
import           System.Directory  (createDirectoryIfMissing, removeDirectoryRecursive)
import           System.FilePath   ((</>))
import           System.IO.Temp    (createTempDirectory, getCanonicalTemporaryDirectory)

testConfig :: FilePath -> Configuration
testConfig tmpDir = defaultConfiguration {
    destinationDirectory = tmpDir </> "_site"
  , storeDirectory = tmpDir </> "_cache"
  , tmpDirectory = tmpDir </> "_tmp"
  , providerDirectory = tmpDir </> "provider"
  }

withTestSite :: (FilePath -> Configuration -> IO a) -> IO a
withTestSite action = do
    tmpBase <- getCanonicalTemporaryDirectory
    bracket
        (createTempDirectory tmpBase "hakyll-test")
        removeDirectoryRecursive
        (\tmpDir -> do
            let cfg = testConfig tmpDir
            createDirectoryIfMissing True (providerDirectory cfg)
            action tmpDir cfg
        )

testCompile :: Configuration -> Rules () -> IO ()
testCompile cfg rules = hakyllWith cfg rules

cleanupTestSite :: FilePath -> IO ()
cleanupTestSite = removeDirectoryRecursive
