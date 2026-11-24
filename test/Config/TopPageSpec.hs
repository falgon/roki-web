{-# LANGUAGE OverloadedStrings #-}
module Config.TopPageSpec (spec) where

import           Config.TopPage
import           Hakyll
import qualified Rules.TopPage    as TopPage
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath  (takeDirectory, (</>))
import           Test.Hspec
import           TestHelpers      (testCompile, withTestSite)

spec :: Spec
spec = do
    describe "topPageConfig" $ do
        it "has correct maxTitleNum" $ do
            maxTitleNum topPageConfig `shouldBe` 4

        it "has correct postDateFormat (URL encoded)" $ do
            postDateFormat topPageConfig `shouldBe` "%Y%%2F%m%%2F%d"

        it "has correct noPostsAlt" $ do
            noPostsAlt topPageConfig `shouldBe` "not yet"

    describe "notFoundRules" $
        it "renders a root 404 page with shared layout, absolute paths and noindex" $
            withTestSite $ \_ cfg -> do
                mapM_ (copyPageFile cfg)
                    [ "contents/pages/404.html"
                    , "contents/templates/site/default.html"
                    , "contents/templates/footer/dec.html"
                    , "contents/templates/footer/copyright.html"
                    ]
                testCompile cfg $ do
                    TopPage.notFoundRules
                    match "contents/templates/**" $ compile templateBodyCompiler
                html <- readFile (destinationDirectory cfg </> "404.html")
                mapM_ (shouldContain html)
                    [ "<meta name=\"robots\" content=\"noindex\" />"
                    , "<header"
                    , "<footer"
                    , "href=\"/\""
                    , "href=\"/roki.log/\""
                    , "href=\"/roki.diary/\""
                    , "href=\"/style/style.css\""
                    , "href=\"/images/avator/favicon.ico\""
                    , "src=\"/js/components.js\""
                    ]

copyPageFile :: Configuration -> FilePath -> IO ()
copyPageFile cfg path = do
    let destination = providerDirectory cfg </> path
    createDirectoryIfMissing True $ takeDirectory destination
    copyFile path destination
