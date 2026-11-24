{-# LANGUAGE OverloadedStrings #-}
module Contexts.BlogSpec (spec) where

import           Config.Program         (readerOptions, tmBlogRoot,
                                         writerPreviewOptions)
import           Config.Site            (baseUrl)
import           Contexts.Blog          (postMetaCtx)
import           Contexts.Core          (siteCtx)
import           Contexts.Field         (breadcrumbField)
import qualified Data.Aeson             as A
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Hakyll
import           System.FilePath        ((</>))
import           Test.Hspec
import           TestHelpers            (testCompile, withTestSite)
import qualified Text.HTML.TagSoup      as TS

type Page = [TS.Tag String]

renderPages :: [String] -> [String] -> IO (Page, Page)
renderPages metadata body = withTestSite $ \_ cfg -> do
    let template = "default.html"
        postSource = "post.md"
        blogRoute = "roki.log/index.html"
        archives = [blogRoute, "roki.log/2024/index.html", "roki.log/2024/01/index.html"]
        postRoute = "roki.log/2024/01/15/sample/index.html"
        blogCtx = mconcat
            [ constField "blog-title" "roki.log"
            , constField "blog-font-html" ""
            , constField "blog-description" ""
            , constField "google-site-verification" ""
            , constField "google-cx" ""
            , siteCtx
            , bodyField "body"
            ]
    readFile (tmBlogRoot </> template) >>= writeFile (providerDirectory cfg </> template)
    writeFile (providerDirectory cfg </> postSource) $ unlines $
        ["---", "title: Sample", "date: 2024-01-15T12:00:00+00:00"]
        ++ metadata ++ ["---"] ++ body
    testCompile cfg $ do
        match (fromGlob template) $ compile templateBodyCompiler
        create (map fromFilePath archives) $ do
            route idRoute
            compile $ makeItem "" >>= loadAndApplyTemplate (fromFilePath template) blogCtx
        match (fromGlob postSource) $ do
            route $ constRoute postRoute
            compile $ do
                post <- pandocCompilerWith readerOptions writerPreviewOptions
                metaCtx <- postMetaCtx post
                let ctx = metaCtx <> breadcrumbField "json-ld-breadcrumb" <> blogCtx
                loadAndApplyTemplate (fromFilePath template) ctx post
    let readPage path = TS.parseTags <$> readFile (destinationDirectory cfg </> path)
    (,) <$> readPage postRoute <*> readPage blogRoute

meta :: String -> Page -> [String]
meta key tags =
    [ TS.fromAttrib "content" tag
    | tag <- tags
    , TS.isTagOpenName "meta" tag
    , any (\attr -> TS.fromAttrib attr tag == key) ["name", "property"]
    ]

jsonField :: A.Key -> A.Value -> Maybe A.Value
jsonField key (A.Object fields) = KM.lookup key fields
jsonField _ _                   = Nothing

jsonLd :: T.Text -> Page -> [A.Value]
jsonLd kind tags =
    [ value
    | (tag, TS.TagText content) <- zip tags (drop 1 tags)
    , TS.isTagOpenName "script" tag
    , TS.fromAttrib "type" tag == "application/ld+json"
    , Just value <- [A.decodeStrict $ TE.encodeUtf8 $ T.pack content]
    , jsonField "@type" value == Just (A.String kind)
    ]

expectDescription :: Page -> String -> Expectation
expectDescription page expected = do
    mapM_ (\key -> meta key page `shouldBe` [expected])
        ["description", "og:description", "twitter:description"]
    map (jsonField "description") (jsonLd "BlogPosting" page)
        `shouldBe` [Just (A.toJSON expected)]

spec :: Spec
spec = describe "blog page metadata" $ do
    it "extracts plain text from Markdown HTML and marks only the article as such" $ do
        (post, listing) <- renderPages ["tags: ['C++ & \"web\"', Haskell]"]
            [ "# Heading", "", "[Link](https://example.com) &amp; &quot;text&quot; &lt;example&gt;"
            , "", "<script>ignored();</script>", "", "foo<em>bar</em><br>after"
            ]
        expectDescription post "Heading Link & \"text\" <example> foobar after"
        meta "og:type" post `shouldBe` ["article"]
        let published = "2024-01-15T21:00:00+09:00" :: String
        meta "article:published_time" post `shouldBe` [published]
        map (jsonField "datePublished") (jsonLd "BlogPosting" post)
            `shouldBe` [Just (A.toJSON published)]
        meta "article:tag" post `shouldBe` ["C++ & \"web\"", "Haskell"]
        meta "og:type" listing `shouldBe` ["website"]
        meta "article:published_time" listing `shouldBe` []
        meta "article:tag" listing `shouldBe` []

    it "uses the explicit description and serializes it safely in HTML and JSON-LD" $ do
        (post, _) <- renderPages ["description: 'Explicit  \"quote\" & </script>'"] ["Ignored body"]
        expectDescription post "Explicit \"quote\" & </script>"
        meta "article:tag" post `shouldBe` []

    it "truncates to 150 Unicode characters and links only to routed breadcrumbs" $ do
        let prefix = replicate 149 '日' ++ "🙂"
        (post, _) <- renderPages [] [prefix ++ "終"]
        expectDescription post prefix
        let crumbUrls =
                [ url
                | schema <- jsonLd "BreadcrumbList" post
                , Just items <- [jsonField "itemListElement" schema]
                , A.Success crumbs <- [A.fromJSON items]
                , Just url <- map (jsonField "item") crumbs
                ]
        crumbUrls `shouldBe` map (A.toJSON . (baseUrl <>))
            [ "", "/roki.log/index.html", "/roki.log/2024/index.html"
            , "/roki.log/2024/01/index.html", "/roki.log/2024/01/15/sample/index.html"
            ]
