{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog.Core (
    BlogConfig (..)
  , blogRules
) where

import           Control.Monad                    (forM_)
import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.Reader             (ask, asks)
import           Control.Monad.Trans              (MonadTrans (..))
import           Hakyll                           hiding
                                                  (FeedConfiguration (..),
                                                   renderAtom, renderRss)
import           System.FilePath                  ((</>))

import           Archives
import           Config
import           Contexts                         (postCtx, siteCtx,
                                                   siteMapDateCtx)
import qualified Contexts.Blog                    as BlogCtx
import           Contexts.Field                   (tagCloudField',
                                                   yearMonthArchiveField)
import           Rules.Blog.EachPostSeries
import qualified Rules.Blog.Feed.Atom             as Atom
import qualified Rules.Blog.Feed.RSS              as RSS
import           Rules.Blog.Footer                (appendFooter)
import qualified Rules.Blog.Index                 as Index
import           Rules.Blog.ListPage              (ListPageOpts (..))
import qualified Rules.Blog.Paginate.MonthlyPosts as MonthlyPosts
import qualified Rules.Blog.Paginate.TaggedPosts  as TaggedPosts
import qualified Rules.Blog.Paginate.YearlyPosts  as YearlyPosts
import qualified Rules.Blog.Search                as Search
import           Rules.Blog.Type
import           Utils                            (absolutizeUrls,
                                                   modifyExternalLinkAttr)
import qualified Vendor.FontAwesome               as FA
import qualified Vendor.KaTeX                     as KaTeX

blogRules :: FA.FontAwesomeIcons -> BlogConfReader Rules Rules ()
blogRules faIcons = do
    bc <- ask
    tags <- asks blogTagBuilder >>= lift
    isPreview <- asks blogIsPreview
    postCtx' <- mconcatMapM id [
        BlogCtx.postCtx tags
      , BlogCtx.tagCloud
      , BlogCtx.title
      , BlogCtx.font
      , BlogCtx.headerAdditionalComponent
      , BlogCtx.beforeContentBodyAdditionalComponent
      , BlogCtx.description
      , BlogCtx.gSuite
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

    listPageOpts <- ListPageOpts
        <$> BlogCtx.title
        <*> asks blogName
        <*> BlogCtx.font
        <*> BlogCtx.description
        <*> BlogCtx.beforeContentBodyAdditionalComponent
        <*> BlogCtx.headerAdditionalComponent
        <*> asks blogContentSnapshot
        <*> BlogCtx.gSuite
        <*> BlogCtx.listCtx
        <*> BlogCtx.postCtx tags

    -- tagged paginate
    TaggedPosts.build faIcons tags listPageOpts

    -- yearly paginate
    yearlyArchives <- YearlyPosts.build faIcons tags listPageOpts

    -- monthly paginate
    monthlyArchives <- MonthlyPosts.build faIcons tags listPageOpts

    -- the index page of blog
    Index.build faIcons tags listPageOpts

    -- footer
    pCtxForFooter <- postCtx tags
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
                        listField "recent-posts" pCtxForFooter (return recent)
                      , yearMonthArchiveField "archives" yearlyArchives monthlyArchives year
                      , footerCtx
                      ]
                makeItem ""
                    >>= loadAndApplyTemplate footerPath ctx
                    >>= relativizeUrls

    -- Atom and RSS Feed
    mapM_ (flip id postCtx' . flip id feedContent) [Atom.build, RSS.build]

    -- Search result page
    Search.build faIcons postCtx'

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
