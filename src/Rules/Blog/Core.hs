{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog.Core (
    BlogConfig (..)
  , blogRules
) where

import           Control.Monad             (forM_)
import           Control.Monad.Extra       (mconcatMapM)
import           Control.Monad.Reader      (ask, asks)
import           Control.Monad.Trans       (MonadTrans (..))
import           Hakyll                    hiding (FeedConfiguration (..),
                                            renderAtom, renderRss)
import           Hakyll.Web.Feed.Extra
import           System.FilePath           (joinPath, (</>))

import           Archives
import           Config
import           Contexts                  (katexJsCtx, postCtx, siteCtx,
                                            siteMapDateCtx)
import qualified Contexts.Blog             as BlogCtx
import           Contexts.Field            (searchBoxResultField,
                                            tagCloudField',
                                            yearMonthArchiveField)
import           Rules.Blog.EachPostSeries
import           Rules.Blog.Footer         (appendFooter)
import           Rules.Blog.ListPage       (ListPageOpts (..), listPage)
import           Rules.Blog.Type
import           Utils                     (absolutizeUrls, makePageIdentifier,
                                            modifyExternalLinkAttr)
import qualified Vendor.FontAwesome        as FA
import qualified Vendor.KaTeX              as KaTeX

blogRules :: FA.FontAwesomeIcons -> BlogConfReader Rules Rules ()
blogRules faIcons = do
    bc <- ask
    tags <- asks blogTagBuilder >>= lift
    isPreview <- asks blogIsPreview
    postCtx' <- mconcatMapM id [
        pure $ postCtx isPreview tags
      , BlogCtx.tagCloud
      , BlogCtx.title
      , BlogCtx.font
      , BlogCtx.headerAdditionalComponent
      , BlogCtx.beforeContentBodyAdditionalComponent
      , BlogCtx.description
      , BlogCtx.gSuite
      , pure $ if isPreview then katexJsCtx else mempty
      ]
    feedContent <- asks $ (<> "-feed-content") . blogName

    -- each posts
    disqusCtx <- mconcatMapM id [
        pure postCtx'
      , BlogCtx.disqus
      ]
    wOptions <- asks blogWriterOptions
    cs <- asks blogContentSnapshot
    blogTitle <- asks blogName

    eachPostsSeries $ \s -> do
        route $ gsubRoute (contentsRoot <> "/") (const mempty) `composeRoutes` setExtension "html"
        compile $ pandocCompilerWith readerOptions wOptions
            >>= absolutizeUrls
            >>= saveSnapshot feedContent
            >>= (if isPreview then return else KaTeX.render)
            >>= saveSnapshot cs
            >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "post.html")
                (s <> disqusCtx)
            >>= appendFooter blogTitle defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "default.html") postCtx'
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons

    lift $ match (blogEntryFilesPattern bc) $ do
        route $ gsubRoute (contentsRoot <> "/") (const mempty)
        compile copyFileCompiler

    peNum <- asks blogPageEntriesNum
    listPageOpts <- ListPageOpts
        <$> BlogCtx.title
        <*> asks blogName
        <*> BlogCtx.font
        <*> BlogCtx.description
        <*> BlogCtx.beforeContentBodyAdditionalComponent
        <*> BlogCtx.headerAdditionalComponent
        <*> asks blogContentSnapshot
        <*> BlogCtx.gSuite
        <*> asks blogIsPreview

    -- tag rules
    lift $ tagsRules tags $ \tag pat ->
        let grouper = fmap (paginateEvery peNum) . sortRecentFirst
            makeId = makePageIdentifier $ blogTagPagesPath bc tag
            title = "Tagged posts: " <> tag
        in buildPaginateWith grouper pat makeId
            >>= listPage (Just title) faIcons tags listPageOpts

    -- yearly paginate
    yearlyArchives <- lift $ blogYearlyArchivesBuilder bc
    lift $ archivesRules yearlyArchives $ \year pat ->
        let grouper = fmap (paginateEvery peNum) . sortRecentFirst
            makeId = makePageIdentifier $ blogYearlyPagePath bc year
            title = "Yearly posts: " <> year
        in buildPaginateWith grouper pat makeId
            >>= listPage (Just title) faIcons tags listPageOpts

    -- monthly paginate
    monthlyArchives <- lift $ blogMonthlyArchivesBuilder bc
    lift $ archivesRules monthlyArchives $ \key@(year, month) pat ->
        let grouper = fmap (paginateEvery peNum) . sortRecentFirst
            makeId = makePageIdentifier $ blogMonthlyPagePath bc key
            title = "Monthly posts: " <> year </> month
        in buildPaginateWith grouper pat makeId
            >>= listPage (Just title) faIcons tags listPageOpts

    -- all tags
    let allTagsPagePath = joinPath [blogName bc, "tags", "index.html"]
    lift $ listPage (Just "tags") faIcons tags listPageOpts =<<
        let grouper = fmap (paginateEvery peNum) . sortRecentFirst
            makeId = makePageIdentifier allTagsPagePath
        in buildPaginateWith grouper (blogEntryPattern bc) makeId

    -- the index page of blog
    lift $ listPage Nothing faIcons tags listPageOpts =<<
        let grouper = fmap (paginateEvery peNum) . sortRecentFirst
            makeId = makePageIdentifier (blogTitle </> "index.html")
        in buildPaginateWith grouper (blogEntryPattern bc) makeId

    -- footer
    footerCtx <- mconcatMapM id [
        pure $ tagCloudField' "tag-cloud" tags
      , pure $ siteCtx
      , BlogCtx.footerAdditionalComponent
      ]
    let footerPath = fromFilePath $ tmBlogRoot </> "footer.html"
    lift $ forM_ (Nothing:map (Just . fst) (archivesMap yearlyArchives)) $ \year -> maybe id version year $
        create [fromFilePath $ blogName bc <> "-footer.html"] $
            compile $ do
                recent <- fmap (take (blogPageEntriesNum bc)) . recentFirst =<<
                    loadAllSnapshots (blogEntryPattern bc) cs
                let ctx = mconcat [
                        listField "recent-posts" (postCtx isPreview tags) (return recent)
                      , yearMonthArchiveField "archives" yearlyArchives monthlyArchives year
                      , footerCtx
                      ]
                makeItem ""
                    >>= loadAndApplyTemplate footerPath ctx
                    >>= relativizeUrls

    feedRecentNum <- asks blogFeedRecentNum

    -- Atom Feed
    lift $ create [fromFilePath (joinPath [blogTitle, "feed", blogTitle <> ".xml"])] $ do
        route idRoute
        compile $
            loadAllSnapshots (blogEntryPattern bc) feedContent
                >>= fmap (take feedRecentNum) . recentFirst
                >>= renderAtom (blogAtomConfig bc) (bodyField "description" <> postCtx')

    -- RSS Feed
    lift $ create [fromFilePath (joinPath [blogTitle, "feed", blogTitle <> "-rss.xml"])] $ do
        route idRoute
        compile $
            loadAllSnapshots (blogEntryPattern bc) feedContent
                >>= fmap (take feedRecentNum) . recentFirst
                >>= renderRss (blogAtomConfig bc) (bodyField "description" <> postCtx')

    -- Search result page
    lift $ create [fromFilePath (blogTitle </> "search.html")] $ do
        route idRoute
        compile $
            makeItem ""
                >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "default.html") (searchBoxResultField <> postCtx')
                >>= absolutizeUrls
                >>= appendFooter blogTitle defaultTimeLocale' timeZoneJST
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons

    -- Site map
    bTitleCtx <- BlogCtx.title
    lift $ create [fromFilePath (blogTitle </> "sitemap.xml")] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots (blogEntryPattern bc) feedContent
            let hostCtx = constField "webroot" ("https://" <> siteName)
                sitemapCtx = mconcat [
                    hostCtx
                  , bTitleCtx
                  , listField "pages" (siteMapDateCtx <> hostCtx <> defaultContext) (return posts)
                  ]
            makeItem ""
                >>= loadAndApplyTemplate
                    (fromFilePath $ tmBlogRoot </> "sitemap.xml")
                        sitemapCtx
