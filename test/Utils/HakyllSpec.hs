module Utils.HakyllSpec (spec) where

import qualified Data.Text              as T
import           Hakyll
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))
import           Test.Hspec
import           TestHelpers
import           Text.Pandoc.Definition
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

    describe "injectTableOfContents" $ do
        it "does not add TOC when no TOC marker exists" $ do
            let doc = Pandoc nullMeta [
                    Header 2 (T.pack "h1", [], []) [Str (T.pack "Header"), Space, Str (T.pack "1")]
                  ]
            let result = injectTableOfContents doc
            result `shouldBe` doc

        it "replaces TOC marker with table of contents" $ do
            let doc = Pandoc nullMeta [
                    RawBlock (Format (T.pack "html")) (T.pack "<!--toc-->")
                  , Header 2 (T.pack "h1", [], []) [Str (T.pack "Section"), Space, Str (T.pack "1")]
                  , Header 2 (T.pack "h2", [], []) [Str (T.pack "Section"), Space, Str (T.pack "2")]
                  ]
            let Pandoc _ blocks = injectTableOfContents doc
            length blocks `shouldBe` 3

        it "includes only headers up to depth 3" $ do
            let doc = Pandoc nullMeta [
                    RawBlock (Format (T.pack "html")) (T.pack "<!--toc-->")
                  , Header 2 (T.pack "h1", [], []) [Str (T.pack "Level"), Space, Str (T.pack "2")]
                  , Header 3 (T.pack "h2", [], []) [Str (T.pack "Level"), Space, Str (T.pack "3")]
                  , Header 4 (T.pack "h3", [], []) [Str (T.pack "Level"), Space, Str (T.pack "4")]
                  ]
            let Pandoc _ blocks = injectTableOfContents doc
            case blocks of
                (Div (tocId, [], []) tocBlocks : _) -> do
                    tocId `shouldBe` T.pack "toc"
                    length tocBlocks `shouldBe` 2
                _ -> expectationFailure "TOC not found"

    describe "modifyExternalLinkAttr" $ do
        it "adds target and rel attributes to external links" $ do
            withTestSite $ \_ cfg -> do
                let provDir = providerDirectory cfg
                let testFile = provDir </> "test.html"
                createDirectoryIfMissing True provDir
                writeFile testFile "<a href=\"https://example.com\">External</a>"

                testCompile cfg $ do
                    match (fromGlob "test.html") $ do
                        route idRoute
                        compile $ getResourceString >>= modifyExternalLinkAttr

                let outputFile = destinationDirectory cfg </> "test.html"
                content <- readFile outputFile
                content `shouldContain` "target=\"_blank\""
                content `shouldContain` "rel=\"nofollow noopener\""

        it "does not modify internal links" $ do
            withTestSite $ \_ cfg -> do
                let provDir = providerDirectory cfg
                let testFile = provDir </> "test.html"
                createDirectoryIfMissing True provDir
                writeFile testFile "<a href=\"/page.html\">Internal</a>"

                testCompile cfg $ do
                    match (fromGlob "test.html") $ do
                        route idRoute
                        compile $ getResourceString >>= modifyExternalLinkAttr

                let outputFile = destinationDirectory cfg </> "test.html"
                content <- readFile outputFile
                content `shouldNotContain` "target=\"_blank\""
                content `shouldNotContain` "rel=\"nofollow noopener\""

        it "handles mixed internal and external links" $ do
            withTestSite $ \_ cfg -> do
                let provDir = providerDirectory cfg
                let testFile = provDir </> "test.html"
                createDirectoryIfMissing True provDir
                writeFile testFile "<a href=\"/page.html\">Internal</a><a href=\"https://example.com\">External</a>"

                testCompile cfg $ do
                    match (fromGlob "test.html") $ do
                        route idRoute
                        compile $ getResourceString >>= modifyExternalLinkAttr

                let outputFile = destinationDirectory cfg </> "test.html"
                content <- readFile outputFile
                content `shouldContain` "target=\"_blank\""

    describe "absolutizeUrls" $ do
        it "converts relative URLs to absolute URLs" $ do
            withTestSite $ \_ cfg -> do
                let provDir = providerDirectory cfg
                let testFile = provDir </> "test.html"
                createDirectoryIfMissing True provDir
                writeFile testFile "<a href=\"page.html\">Link</a>"

                testCompile cfg $ do
                    match (fromGlob "test.html") $ do
                        route idRoute
                        compile $ getResourceString >>= absolutizeUrls

                let outputFile = destinationDirectory cfg </> "test.html"
                content <- readFile outputFile
                content `shouldContain` "href=\"/page.html\""

        it "preserves absolute URLs" $ do
            withTestSite $ \_ cfg -> do
                let provDir = providerDirectory cfg
                let testFile = provDir </> "test.html"
                createDirectoryIfMissing True provDir
                writeFile testFile "<a href=\"/absolute.html\">Link</a>"

                testCompile cfg $ do
                    match (fromGlob "test.html") $ do
                        route idRoute
                        compile $ getResourceString >>= absolutizeUrls

                let outputFile = destinationDirectory cfg </> "test.html"
                content <- readFile outputFile
                content `shouldContain` "href=\"/absolute.html\""

        it "preserves external URLs" $ do
            withTestSite $ \_ cfg -> do
                let provDir = providerDirectory cfg
                let testFile = provDir </> "test.html"
                createDirectoryIfMissing True provDir
                writeFile testFile "<a href=\"https://example.com\">Link</a>"

                testCompile cfg $ do
                    match (fromGlob "test.html") $ do
                        route idRoute
                        compile $ getResourceString >>= absolutizeUrls

                let outputFile = destinationDirectory cfg </> "test.html"
                content <- readFile outputFile
                content `shouldContain` "href=\"https://example.com\""
