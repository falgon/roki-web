{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog.Core (
    BlogConfig (..)
  , rules
) where

import           Control.Monad.Reader             (asks)
import           Control.Monad.Trans              (MonadTrans (..))
import           Hakyll                           hiding
                                                  (FeedConfiguration (..),
                                                   renderAtom, renderRss)

import           Config
import qualified Contexts.Blog                    as BlogCtx
import           Rules.Blog.EachPosts             as EachPosts
import qualified Rules.Blog.Feed.Atom             as Atom
import qualified Rules.Blog.Feed.RSS              as RSS
import qualified Rules.Blog.Footer                as Footer
import qualified Rules.Blog.Index                 as Index
import           Rules.Blog.ListPage              (ListPageOpts (..))
import qualified Rules.Blog.Paginate.MonthlyPosts as MonthlyPosts
import qualified Rules.Blog.Paginate.TaggedPosts  as TaggedPosts
import qualified Rules.Blog.Paginate.YearlyPosts  as YearlyPosts
import qualified Rules.Blog.Search                as Search
import           Rules.Blog.Sitemap               as Sitemap
import           Rules.Blog.Type
import           Utils                            (mconcatM)
import qualified Vendor.FontAwesome               as FA

rules :: FA.FontAwesomeIcons -> BlogConfReader Rules Rules ()
rules faIcons = do
    tags <- asks blogTagBuilder >>= lift
    postCtx' <- mconcatM [
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
    Footer.build tags yearlyArchives monthlyArchives

    -- Atom and RSS Feed
    mapM_ (flip id postCtx' . flip id feedContent) [Atom.build, RSS.build]

    -- Search result page
    Search.build faIcons postCtx'

    -- Site map
    Sitemap.build feedContent
