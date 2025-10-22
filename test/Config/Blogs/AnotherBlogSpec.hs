{-# LANGUAGE OverloadedStrings #-}
module Config.Blogs.AnotherBlogSpec (spec) where

import           Config.Blogs.AnotherBlog
import           Hakyll                   (fromFilePath, matches)
import           Hakyll.Web.Feed.Extra    (FeedConfiguration (..))
import           Test.Hspec

spec :: Spec
spec = do
    describe "blogName" $ do
        it "returns the correct blog name" $ do
            blogName `shouldBe` "roki.diary"

    describe "blogDesc" $ do
        it "generates HTML description with link" $ do
            blogDesc `shouldContain` "<a href=\"/roki.diary\">roki.diary</a>"

        it "contains description text" $ do
            blogDesc `shouldContain` "is just a diary"

    describe "entryPattern" $ do
        it "matches valid blog post paths" $ do
            matches entryPattern (fromFilePath "contents/roki.diary/2024/01/15/test-post/index.md") `shouldBe` True

        it "does not match non-index files" $ do
            matches entryPattern (fromFilePath "contents/roki.diary/2024/01/15/test-post/other.md") `shouldBe` False

    describe "entryFilesPattern" $ do
        it "matches all files in blog post directories" $ do
            matches entryFilesPattern (fromFilePath "contents/roki.diary/2024/01/15/test-post/image.png") `shouldBe` True

    describe "contentSnapshot" $ do
        it "returns correct snapshot name" $ do
            contentSnapshot `shouldBe` "roki.diary.content"

    describe "feedConfig" $ do
        it "has correct feedTitle" $ do
            feedTitle feedConfig `shouldBe` "roki.diary"

        it "has correct feedWebRoot" $ do
            feedWebRoot feedConfig `shouldBe` "https://roki.dev"

        it "has correct feedBlogName" $ do
            feedBlogName feedConfig `shouldBe` "roki.diary"

        it "has correct feedDescription" $ do
            feedDescription feedConfig `shouldBe` "Roki Diary"

        it "has correct feedAuthorName" $ do
            feedAuthorName feedConfig `shouldBe` "Roki"

        it "has correct feedAuthorEmail" $ do
            feedAuthorEmail feedConfig `shouldBe` "falgon53@yahoo.co.jp"

    describe "tagPagesPath" $ do
        it "generates correct tag page path" $ do
            tagPagesPath "daily" `shouldBe` "roki.diary/tags/daily/index.html"

    describe "yearlyPagePath" $ do
        it "generates correct yearly page path" $ do
            yearlyPagePath "2024" `shouldBe` "roki.diary/2024/index.html"

    describe "monthlyPagePath" $ do
        it "generates correct monthly page path" $ do
            monthlyPagePath ("2024", "01") `shouldBe` "roki.diary/2024/01/index.html"
