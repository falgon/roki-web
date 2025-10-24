module Utils.BaseSpec (spec) where

import           Test.Hspec
import           Utils.Base

spec :: Spec
spec = do
    describe "mconcatM" $ do
        it "concatenates monadic values with Monoid instance" $ do
            result <- mconcatM [pure [1, 2], pure [3, 4], pure [5 :: Int]]
            result `shouldBe` [1, 2, 3, 4, 5]

        it "returns mempty for empty list" $ do
            result <- mconcatM ([] :: [IO [Int]])
            result `shouldBe` []

        it "works with single element" $ do
            result <- mconcatM [pure [42 :: Int]]
            result `shouldBe` [42]

        it "works with String concatenation" $ do
            result <- mconcatM [pure "Hello", pure " ", pure "World"]
            result `shouldBe` "Hello World"
