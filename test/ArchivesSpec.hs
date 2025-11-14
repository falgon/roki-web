{-# LANGUAGE OverloadedStrings #-}

module ArchivesSpec (spec) where

import           Archives
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Data.List               (sortOn)
import           Data.Time.Format        (defaultTimeLocale)
import           Data.Time.LocalTime     (utc)
import           Hakyll
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (takeDirectory, (</>))
import           Test.Hspec
import           TestHelpers             (testCompile, withTestSite)

patternPath :: Pattern
patternPath = fromGlob "posts/**/*"

writePost ::
       FilePath
    -> String
    -> IO ()
writePost path dateValue = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path $ unlines
        [ "---"
        , "title: Sample"
        , "date: " <> dateValue
        , "---"
        , "content"
        ]

makeArchiveId :: String -> Identifier
makeArchiveId year = fromFilePath $ "archives/" <> year <> ".html"

makeMonthlyArchiveId :: (String, String) -> Identifier
makeMonthlyArchiveId (year, month) =
    fromFilePath $ "archives/" <> year <> "/" <> month <> ".html"

spec :: Spec
spec = describe "Archives" $ do
    it "buildYearlyArchives groups posts by year" $ withTestSite $ \_ cfg -> do
        let providerDir = providerDirectory cfg
        writePost (providerDir </> "posts/2024/first.md") "2024-01-15T12:00:00+00:00"
        writePost (providerDir </> "posts/2024/second.md") "2024-06-10T12:00:00+00:00"
        writePost (providerDir </> "posts/2023/third.md") "2023-03-05T12:00:00+00:00"

        resultVar <- newEmptyMVar

        testCompile cfg $ do
            match "posts/**/*.md" $ compile getResourceBody
            create ["archives-collect"] $ compile $ do
                archives <- buildYearlyArchives defaultTimeLocale utc patternPath makeArchiveId
                unsafeCompiler $ putMVar resultVar archives
                makeItem ("" :: String)

        archives <- takeMVar resultVar
        let ordered = sortOn fst $ archivesMap archives
        ordered `shouldBe`
            [ ("2023", [fromFilePath "posts/2023/third.md"])
            , ("2024", [ fromFilePath "posts/2024/first.md"
                       , fromFilePath "posts/2024/second.md"
                       ])
            ]
        archivesMakeId archives "2024" `shouldBe` fromFilePath "archives/2024.html"

    it "buildMonthlyArchives groups posts by month" $ withTestSite $ \_ cfg -> do
        let providerDir = providerDirectory cfg
        writePost (providerDir </> "posts/2024/january.md") "2024-01-15T12:00:00+00:00"
        writePost (providerDir </> "posts/2024/february.md") "2024-02-18T12:00:00+00:00"

        resultVar <- newEmptyMVar

        testCompile cfg $ do
            match "posts/**/*.md" $ compile getResourceBody
            create ["monthly-collect"] $ compile $ do
                archives <- buildMonthlyArchives defaultTimeLocale utc patternPath makeMonthlyArchiveId
                unsafeCompiler $ putMVar resultVar archives
                makeItem ("" :: String)

        archives <- takeMVar resultVar
        let ordered = sortOn fst $ archivesMap archives
        ordered `shouldBe`
            [ (("2024", "01"), [fromFilePath "posts/2024/january.md"])
            , (("2024", "02"), [fromFilePath "posts/2024/february.md"])
            ]
        archivesMakeId archives ("2024", "02")
            `shouldBe` fromFilePath "archives/2024/02.html"

    it "archivesRules creates pages for each archive entry" $ withTestSite $ \_ cfg -> do
        let archives = Archives
                { archivesMap =
                    [ ("2024", [fromFilePath "posts/2024/first.md"])
                    , ("2023", [fromFilePath "posts/2023/second.md"])
                    ]
                , archivesMakeId = makeArchiveId
                , archivesDependency = PatternDependency patternPath mempty
                }

        testCompile cfg $ do
            archivesRules archives $ \key _ -> do
                route idRoute
                compile $ makeItem (key <> " archive" :: String)

        content2024 <- readFile $ destinationDirectory cfg </> "archives/2024.html"
        content2023 <- readFile $ destinationDirectory cfg </> "archives/2023.html"
        content2024 `shouldContain` "2024 archive"
        content2023 `shouldContain` "2023 archive"
