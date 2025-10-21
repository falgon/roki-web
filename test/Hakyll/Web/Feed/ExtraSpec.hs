module Hakyll.Web.Feed.ExtraSpec (spec) where

import           Hakyll.Web.Feed.Extra
import           Test.Hspec

spec :: Spec
spec = do
    describe "FeedConfiguration" $ do
        it "can be constructed with all fields" $ do
            let config = FeedConfiguration {
                    feedTitle = "Test Blog"
                  , feedWebRoot = "https://example.com"
                  , feedBlogName = "Test"
                  , feedDescription = "A test blog"
                  , feedAuthorName = "Test Author"
                  , feedAuthorEmail = "test@example.com"
                  }
            feedTitle config `shouldBe` "Test Blog"
            feedWebRoot config `shouldBe` "https://example.com"
            feedBlogName config `shouldBe` "Test"
            feedDescription config `shouldBe` "A test blog"
            feedAuthorName config `shouldBe` "Test Author"
            feedAuthorEmail config `shouldBe` "test@example.com"

        it "supports Eq instance" $ do
            let config1 = FeedConfiguration "Title" "Root" "Name" "Desc" "Author" "Email"
            let config2 = FeedConfiguration "Title" "Root" "Name" "Desc" "Author" "Email"
            let config3 = FeedConfiguration "Other" "Root" "Name" "Desc" "Author" "Email"
            config1 `shouldBe` config2
            config1 `shouldNotBe` config3

        it "supports Show instance" $ do
            let config = FeedConfiguration "T" "R" "N" "D" "A" "E"
            show config `shouldContain` "FeedConfiguration"
            show config `shouldContain` "T"
