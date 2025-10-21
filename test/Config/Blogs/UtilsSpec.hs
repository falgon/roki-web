module Config.Blogs.UtilsSpec (spec) where

import           Config.Blogs.Utils
import           Test.Hspec

spec :: Spec
spec = do
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
