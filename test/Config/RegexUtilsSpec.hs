module Config.RegexUtilsSpec (spec) where

import           Config.RegexUtils
import           Test.Hspec
import           Text.Regex.TDFA

matchesExactly :: String -> String -> Bool
matchesExactly text pattern = text =~ ("^" ++ pattern ++ "$")

spec :: Spec
spec = do
    describe "yyyy" $ do
        it "matches valid years from 1000 to 2999" $ do
            matchesExactly "1000" yyyy `shouldBe` True
            matchesExactly "1999" yyyy `shouldBe` True
            matchesExactly "2000" yyyy `shouldBe` True
            matchesExactly "2024" yyyy `shouldBe` True
            matchesExactly "2999" yyyy `shouldBe` True

        it "does not match invalid years" $ do
            matchesExactly "0999" yyyy `shouldBe` False
            matchesExactly "3000" yyyy `shouldBe` False
            matchesExactly "999" yyyy `shouldBe` False
            matchesExactly "20240" yyyy `shouldBe` False

    describe "mm" $ do
        it "matches valid months 01-12" $ do
            matchesExactly "01" mm `shouldBe` True
            matchesExactly "1" mm `shouldBe` True
            matchesExactly "09" mm `shouldBe` True
            matchesExactly "12" mm `shouldBe` True

        it "does not match invalid months" $ do
            matchesExactly "00" mm `shouldBe` False
            matchesExactly "13" mm `shouldBe` False
            matchesExactly "99" mm `shouldBe` False

    describe "dd" $ do
        it "matches valid days 01-31" $ do
            matchesExactly "01" dd `shouldBe` True
            matchesExactly "1" dd `shouldBe` True
            matchesExactly "15" dd `shouldBe` True
            matchesExactly "31" dd `shouldBe` True

        it "does not match invalid days" $ do
            matchesExactly "00" dd `shouldBe` False
            matchesExactly "32" dd `shouldBe` False
            matchesExactly "99" dd `shouldBe` False
