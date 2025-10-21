{-# LANGUAGE OverloadedStrings #-}
module Config.Blogs.TechBlogSpec (spec) where

import           Config.Blogs.TechBlog
import           Hakyll.Web.Feed.Extra (FeedConfiguration (..))
import           Test.Hspec

spec :: Spec
spec = do
    describe "blogName" $ do
        it "returns the correct blog name" $ do
            blogName `shouldBe` "roki.log"

    describe "blogDesc" $ do
        it "generates HTML description with link" $ do
            blogDesc `shouldContain` "<a href=\"/roki.log\">roki.log</a>"

        it "contains description text" $ do
            blogDesc `shouldContain` "efforts and learning related to technology"

    describe "contentSnapshot" $ do
        it "returns correct snapshot name" $ do
            contentSnapshot `shouldBe` "roki.log.content"

    describe "feedConfig" $ do
        it "has correct feedTitle" $ do
            feedTitle feedConfig `shouldBe` "roki.log"

        it "has correct feedWebRoot" $ do
            feedWebRoot feedConfig `shouldBe` "https://roki.dev"

        it "has correct feedBlogName" $ do
            feedBlogName feedConfig `shouldBe` "roki.log"

        it "has correct feedDescription" $ do
            feedDescription feedConfig `shouldBe` "Roki tech blog"

        it "has correct feedAuthorName" $ do
            feedAuthorName feedConfig `shouldBe` "Roki"

        it "has correct feedAuthorEmail" $ do
            feedAuthorEmail feedConfig `shouldBe` "falgon53@yahoo.co.jp"

    describe "tagPagesPath" $ do
        it "generates correct tag page path" $ do
            tagPagesPath "haskell" `shouldBe` "roki.log/tags/haskell/index.html"

    describe "yearlyPagePath" $ do
        it "generates correct yearly page path" $ do
            yearlyPagePath "2024" `shouldBe` "roki.log/2024/index.html"

    describe "monthlyPagePath" $ do
        it "generates correct monthly page path" $ do
            monthlyPagePath ("2024", "01") `shouldBe` "roki.log/2024/01/index.html"
