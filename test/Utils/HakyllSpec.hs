module Utils.HakyllSpec (spec) where

import           Hakyll
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

    describe "makePageIdentifier" $ do
        it "returns original path for page 1" $ do
            let result = makePageIdentifier "posts/index.html" 1
            toFilePath result `shouldBe` "posts/index.html"

        it "creates page directory for page 2" $ do
            let result = makePageIdentifier "posts/index.html" 2
            toFilePath result `shouldBe` "posts/page/2/index.html"

        it "creates page directory for page 10" $ do
            let result = makePageIdentifier "posts/index.html" 10
            toFilePath result `shouldBe` "posts/page/10/index.html"

        it "handles root path for page 1" $ do
            let result = makePageIdentifier "index.html" 1
            toFilePath result `shouldBe` "index.html"

        it "handles root path for page 2" $ do
            let result = makePageIdentifier "index.html" 2
            toFilePath result `shouldBe` "page/2/index.html"

        it "handles nested directories" $ do
            let result = makePageIdentifier "blog/tech/posts.html" 3
            toFilePath result `shouldBe` "blog/tech/page/3/posts.html"
