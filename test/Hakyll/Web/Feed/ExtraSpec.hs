{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Feed.ExtraSpec (spec) where

import           Data.List             (isInfixOf)
import           Hakyll                hiding (FeedConfiguration,
                                        feedAuthorEmail, feedAuthorName,
                                        feedDescription, feedTitle, renderAtom,
                                        renderRss)
import           Hakyll.Web.Feed.Extra
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       (takeDirectory, (</>))
import           Test.Hspec
import           TestHelpers           (testCompile, withTestSite)

feedConfig :: FeedConfiguration
feedConfig =
    FeedConfiguration
        { feedTitle = "Test Feed"
        , feedWebRoot = "https://example.com"
        , feedBlogName = "blog"
        , feedDescription = "Feed description"
        , feedAuthorName = "Author"
        , feedAuthorEmail = "author@example.com"
        }

postContext :: Context String
postContext = mconcat
    [ bodyField "description"
    , metadataField
    , urlField "url"
    , pathField "path"
    ]

writeEntry :: FilePath -> (String, String) -> IO ()
writeEntry path (publishedDate, updatedDate) = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path $ unlines
        [ "---"
        , "title: Entry"
        , "description: \"Body with ]]> inside\""
        , "published: " <> publishedDate
        , "updated: " <> updatedDate
        , "---"
        , "content"
        ]

spec :: Spec
spec = describe "Hakyll.Web.Feed.Extra" $ do
    it "renders RSS and Atom feeds with escaped CDATA" $ withTestSite $ \_ cfg -> do
        let providerDir = providerDirectory cfg
        writeEntry (providerDir </> "posts/entry1.md")
            ("2024-01-15T12:00:00+00:00", "2024-01-16T15:00:00+00:00")

        testCompile cfg $ do
            match "posts/*.md" $ do
                route $ setExtension "html"
                compile $ makeItem ("Body with ]]> inside" :: String)

            create ["rss.xml"] $ do
                route idRoute
                compile $ do
                    items <- loadAll "posts/*.md"
                    renderRss feedConfig postContext items

            create ["atom.xml"] $ do
                route idRoute
                compile $ do
                    items <- loadAll "posts/*.md"
                    renderAtom feedConfig postContext items

        rss <- readFile $ destinationDirectory cfg </> "rss.xml"
        atom <- readFile $ destinationDirectory cfg </> "atom.xml"
        rss `shouldSatisfy` ("Body with ]]&gt; inside" `isInfixOf`)
        atom `shouldSatisfy` ("Body with ]]&gt; inside" `isInfixOf`)
        rss `shouldSatisfy` ("<dc:creator>Author</dc:creator>" `isInfixOf`)
        atom `shouldSatisfy` ("<title>Entry</title>" `isInfixOf`)

    it "renders feeds with Unknown timestamp when no entries" $ withTestSite $ \_ cfg -> do
        testCompile cfg $ do
            create ["rss-empty.xml"] $ do
                route idRoute
                compile $ renderRss feedConfig postContext []
            create ["atom-empty.xml"] $ do
                route idRoute
                compile $ renderAtom feedConfig postContext []

        rssEmpty <- readFile $ destinationDirectory cfg </> "rss-empty.xml"
        atomEmpty <- readFile $ destinationDirectory cfg </> "atom-empty.xml"
        rssEmpty `shouldSatisfy` ("<lastBuildDate>Unknown</lastBuildDate>" `isInfixOf`)
        atomEmpty `shouldSatisfy` ("<updated>Unknown</updated>" `isInfixOf`)
