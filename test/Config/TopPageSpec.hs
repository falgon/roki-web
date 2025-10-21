module Config.TopPageSpec (spec) where

import           Config.TopPage
import           Test.Hspec

spec :: Spec
spec = do
    describe "topPageConfig" $ do
        it "has correct maxTitleNum" $ do
            maxTitleNum topPageConfig `shouldBe` 4

        it "has correct postDateFormat (URL encoded)" $ do
            postDateFormat topPageConfig `shouldBe` "%Y%%2F%m%%2F%d"

        it "has correct noPostsAlt" $ do
            noPostsAlt topPageConfig `shouldBe` "not yet"
