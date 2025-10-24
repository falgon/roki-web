module Config.Blogs.UtilsSpec (spec) where

import           Archives                (archivesMakeId, archivesMap)
import           Config.Blogs.Utils
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Data.Bifunctor          (second)
import           Data.List               (sortOn)
import           Hakyll                  hiding (buildTags)
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (takeDirectory, (</>))
import           Test.Hspec
import           TestHelpers             (testCompile, withTestSite)

writePost ::
       FilePath
    -> FilePath
    -> [String]
    -> [String]
    -> IO ()
writePost provider relative metadata body = do
    let path = provider </> relative
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path $ unlines $ ["---"] <> metadata <> ["---"] <> body

spec :: Spec
spec = do
    describe "entryPattern" $ do
        it "matches valid blog post paths" $ do
            let pattern = entryPattern "roki.log"
            matches pattern (fromFilePath "contents/roki.log/2024/01/15/test-post/index.md") `shouldBe` True

        it "does not match non-index files" $ do
            let pattern = entryPattern "roki.log"
            matches pattern (fromFilePath "contents/roki.log/2024/01/15/test-post/other.md") `shouldBe` False

        it "does not match files without date structure" $ do
            let pattern = entryPattern "roki.log"
            matches pattern (fromFilePath "contents/roki.log/test-post/index.md") `shouldBe` False

    describe "entryFilesPattern" $ do
        it "matches all files in blog post directories" $ do
            let pattern = entryFilesPattern "roki.log"
            matches pattern (fromFilePath "contents/roki.log/2024/01/15/test-post/image.png") `shouldBe` True

        it "matches index.md files" $ do
            let pattern = entryFilesPattern "roki.log"
            matches pattern (fromFilePath "contents/roki.log/2024/01/15/test-post/index.md") `shouldBe` True

        it "does not match files outside date structure" $ do
            let pattern = entryFilesPattern "roki.log"
            matches pattern (fromFilePath "contents/roki.log/other.txt") `shouldBe` False

    describe "contentSnapshot" $ do
        it "generates correct snapshot name" $ do
            contentSnapshot "roki.log" `shouldBe` "roki.log.content"

        it "handles different blog names" $ do
            contentSnapshot "roki.diary" `shouldBe` "roki.diary.content"

    describe "tagPagesPath" $ do
        it "generates correct path for simple tag" $ do
            tagPagesPath "roki.log" "haskell" `shouldBe` "roki.log/tags/haskell/index.html"

        it "sanitizes tag names with spaces" $ do
            tagPagesPath "roki.log" "functional programming" `shouldBe` "roki.log/tags/functional-programming/index.html"

        it "sanitizes tag names with uppercase" $ do
            tagPagesPath "roki.log" "Haskell" `shouldBe` "roki.log/tags/haskell/index.html"

        it "sanitizes tag names with special characters" $ do
            tagPagesPath "roki.log" "C++" `shouldBe` "roki.log/tags/c/index.html"

        it "handles different blog names" $ do
            tagPagesPath "diary" "test" `shouldBe` "diary/tags/test/index.html"

    describe "yearlyPagePath" $ do
        it "generates correct path for year" $ do
            yearlyPagePath "roki.log" "2024" `shouldBe` "roki.log/2024/index.html"

        it "handles different years" $ do
            yearlyPagePath "roki.log" "2023" `shouldBe` "roki.log/2023/index.html"

        it "handles different blog names" $ do
            yearlyPagePath "diary" "2024" `shouldBe` "diary/2024/index.html"

    describe "monthlyPagePath" $ do
        it "generates correct path for year and month" $ do
            monthlyPagePath "roki.log" ("2024", "01") `shouldBe` "roki.log/2024/01/index.html"

        it "handles different months" $ do
            monthlyPagePath "roki.log" ("2024", "12") `shouldBe` "roki.log/2024/12/index.html"

        it "handles different blog names" $ do
            monthlyPagePath "diary" ("2024", "06") `shouldBe` "diary/2024/06/index.html"

        it "handles single digit months" $ do
            monthlyPagePath "roki.log" ("2024", "3") `shouldBe` "roki.log/2024/3/index.html"

    describe "buildTags" $ do
        it "collects tags and generates identifiers" $
            withTestSite $ \_ cfg -> do
                let providerDir = providerDirectory cfg
                let post1 = "contents/roki.log/2024/01/15/first/index.md"
                let post2 = "contents/roki.log/2024/02/10/second/index.md"
                writePost providerDir post1
                    [ "title: First"
                    , "date: 2024-01-15T12:00:00+00:00"
                    , "tags: [haskell, diary]"
                    ]
                    ["First post"]
                writePost providerDir post2
                    [ "title: Second"
                    , "date: 2024-02-10T12:00:00+00:00"
                    , "tags: [haskell]"
                    ]
                    ["Second post"]

                resultVar <- newEmptyMVar
                testCompile cfg $ do
                    match (fromGlob "contents/**") $ compile getResourceBody
                    create [fromFilePath "collect-tags"] $ compile $ do
                        tags <- buildTags "roki.log"
                        unsafeCompiler $ putMVar resultVar tags
                        makeItem ("" :: String)

                tags <- takeMVar resultVar
                let actual = sortOn fst $ map (second (sortOn toFilePath)) (tagsMap tags)
                let expected =
                        [ ("diary", [fromFilePath post1])
                        , ("haskell", [fromFilePath post1, fromFilePath post2])
                        ]
                sortOn fst (map (second (sortOn toFilePath)) expected) `shouldBe` actual
                toFilePath (tagsMakeId tags "haskell") `shouldBe` "roki.log/tags/haskell/index.html"

    describe "buildYearlyArchives" $ do
        it "builds archives grouped by year" $
            withTestSite $ \_ cfg -> do
                let providerDir = providerDirectory cfg
                let post1 = "contents/roki.log/2024/01/15/first/index.md"
                let post2 = "contents/roki.log/2023/12/30/second/index.md"
                writePost providerDir post1
                    [ "title: First"
                    , "date: 2024-01-15T12:00:00+00:00"
                    ]
                    ["First post"]
                writePost providerDir post2
                    [ "title: Second"
                    , "date: 2023-12-30T09:00:00+00:00"
                    ]
                    ["Second post"]

                resultVar <- newEmptyMVar
                testCompile cfg $ do
                    match (fromGlob "contents/**") $ compile getResourceBody
                    create [fromFilePath "collect-yearly"] $ compile $ do
                        archives <- buildYearlyArchives "roki.log"
                        unsafeCompiler $ putMVar resultVar archives
                        makeItem ("" :: String)

                archives <- takeMVar resultVar
                let actual = sortOn fst (archivesMap archives)
                actual `shouldBe`
                    [ ("2023", [fromFilePath post2])
                    , ("2024", [fromFilePath post1])
                    ]
                toFilePath (archivesMakeId archives "2024") `shouldBe` "roki.log/2024/index.html"

    describe "buildMonthlyArchives" $ do
        it "builds archives grouped by year and month" $
            withTestSite $ \_ cfg -> do
                let providerDir = providerDirectory cfg
                let post1 = "contents/roki.log/2024/01/05/first/index.md"
                let post2 = "contents/roki.log/2024/02/10/second/index.md"
                writePost providerDir post1
                    [ "title: First"
                    , "date: 2024-01-05T08:00:00+00:00"
                    ]
                    ["First post"]
                writePost providerDir post2
                    [ "title: Second"
                    , "date: 2024-02-10T11:00:00+00:00"
                    ]
                    ["Second post"]

                resultVar <- newEmptyMVar
                testCompile cfg $ do
                    match (fromGlob "contents/**") $ compile getResourceBody
                    create [fromFilePath "collect-monthly"] $ compile $ do
                        archives <- buildMonthlyArchives "roki.log"
                        unsafeCompiler $ putMVar resultVar archives
                        makeItem ("" :: String)

                archives <- takeMVar resultVar
                let actual = sortOn fst (archivesMap archives)
                actual `shouldBe`
                    [ (("2024", "01"), [fromFilePath post1])
                    , (("2024", "02"), [fromFilePath post2])
                    ]
                toFilePath (archivesMakeId archives ("2024", "02")) `shouldBe` "roki.log/2024/02/index.html"
