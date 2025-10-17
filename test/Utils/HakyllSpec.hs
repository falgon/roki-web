module Utils.HakyllSpec (spec) where

import           Test.Hspec
import           Utils.Hakyll

spec :: Spec
spec = do
    describe "sanitizeTagName" $ do
        it "converts spaces to hyphens" $
            sanitizeTagName "hello world" `shouldBe` "hello-world"

        it "converts uppercase to lowercase" $
            sanitizeTagName "Hello World" `shouldBe` "hello-world"

        it "filters out invalid characters" $
            sanitizeTagName "hello@world#test" `shouldBe` "helloworldtest"

        it "keeps alphanumeric characters" $
            sanitizeTagName "test123" `shouldBe` "test123"

        it "keeps hyphens and underscores" $
            sanitizeTagName "test-name_123" `shouldBe` "test-name_123"

        it "handles multiple consecutive spaces" $
            sanitizeTagName "hello   world" `shouldBe` "hello---world"

        it "handles mixed case with special chars" $
            sanitizeTagName "Hello World! 123" `shouldBe` "hello-world-123"

    describe "sanitizeDisqusName" $ do
        it "replaces dots with hyphens" $
            sanitizeDisqusName "hello.world" `shouldBe` "hello-world"

        it "replaces multiple dots" $
            sanitizeDisqusName "a.b.c.d" `shouldBe` "a-b-c-d"

        it "keeps other characters unchanged" $
            sanitizeDisqusName "hello_world-123" `shouldBe` "hello_world-123"

        it "handles empty string" $
            sanitizeDisqusName "" `shouldBe` ""

        it "handles string with no dots" $
            sanitizeDisqusName "helloworld" `shouldBe` "helloworld"
