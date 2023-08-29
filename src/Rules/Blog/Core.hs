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
import           Contexts                         (postCtx, siteCtx)
import qualified Contexts.Blog                    as BlogCtx
import           Contexts.Field                   (tagCloudField',
                                                   yearMonthArchiveField)
import           Rules.Blog.EachPosts             as EachPosts
import qualified Rules.Blog.Feed.Atom             as Atom
import qualified Rules.Blog.Feed.RSS              as RSS
import qualified Rules.Blog.Index                 as Index
import           Rules.Blog.ListPage              (ListPageOpts (..))
import qualified Rules.Blog.Paginate.MonthlyPosts as MonthlyPosts
import qualified Rules.Blog.Paginate.TaggedPosts  as TaggedPosts
import qualified Rules.Blog.Paginate.YearlyPosts  as YearlyPosts
import qualified Rules.Blog.Search                as Search
import           Rules.Blog.Sitemap               as Sitemap
import           Rules.Blog.Type
import qualified Vendor.FontAwesome               as FA

blogRules :: FA.FontAwesomeIcons -> BlogConfReader Rules Rules ()
blogRules faIcons = do
    bc <- ask
    tags <- asks blogTagBuilder >>= lift
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

    -- each posts
    feedContent <- EachPosts.build faIcons postCtx'

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
    cs <- asks blogContentSnapshot
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
    Sitemap.build feedContent
