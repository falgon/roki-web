module Config.ProgramSpec (spec) where

import           Config.Program
import           Hakyll              (destinationDirectory, inMemoryCache,
                                      previewHost, previewPort, storeDirectory,
                                      tmpDirectory)
import           Test.Hspec
import           Text.Pandoc.Options (Extension (..), HTMLMathMethod (..),
                                      ReaderOptions (..), WriterOptions (..),
                                      extensionEnabled)

spec :: Spec
spec = do
    describe "contentsRoot" $ do
        it "returns the correct contents root path" $ do
            contentsRoot `shouldBe` "contents"

    describe "templatesRoot" $ do
        it "returns the correct templates root path" $ do
            templatesRoot `shouldBe` "contents/templates"

    describe "tmBlogRoot" $ do
        it "returns the correct blog templates root path" $ do
            tmBlogRoot `shouldBe` "contents/templates/blog"

    describe "hakyllConfig" $ do
        it "has correct preview host" $ do
            previewHost hakyllConfig `shouldBe` "127.0.0.1"

        it "has correct preview port" $ do
            previewPort hakyllConfig `shouldBe` 8888

        it "has correct destination directory" $ do
            destinationDirectory hakyllConfig `shouldBe` "docs"

        it "has correct store directory" $ do
            storeDirectory hakyllConfig `shouldBe` ".cache"

        it "has correct tmp directory" $ do
            tmpDirectory hakyllConfig `shouldBe` ".cache/tmp"

        it "has inMemoryCache enabled" $ do
            inMemoryCache hakyllConfig `shouldBe` True

    describe "readerOptions" $ do
        it "has Ext_raw_html extension enabled" $ do
            extensionEnabled Ext_raw_html (readerExtensions readerOptions) `shouldBe` True

        it "has Ext_east_asian_line_breaks extension enabled" $ do
            extensionEnabled Ext_east_asian_line_breaks (readerExtensions readerOptions) `shouldBe` True

        it "has Ext_emoji extension enabled" $ do
            extensionEnabled Ext_emoji (readerExtensions readerOptions) `shouldBe` True

        it "has Ext_tex_math_double_backslash extension enabled" $ do
            extensionEnabled Ext_tex_math_double_backslash (readerExtensions readerOptions) `shouldBe` True

        it "has Ext_citations extension disabled" $ do
            extensionEnabled Ext_citations (readerExtensions readerOptions) `shouldBe` False

    describe "writerOptions" $ do
        it "uses KaTeX for math rendering" $ do
            case writerHTMLMathMethod writerOptions of
                KaTeX _ -> return ()
                _       -> expectationFailure "Expected KaTeX math method"

    describe "writerPreviewOptions" $ do
        it "uses MathJax for math rendering" $ do
            case writerHTMLMathMethod writerPreviewOptions of
                MathJax _ -> return ()
                _         -> expectationFailure "Expected MathJax math method"
