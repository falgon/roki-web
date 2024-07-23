{-# LANGUAGE OverloadedStrings #-}
module Rules.TopPage (rules) where

import           Control.Monad.Extra  (mconcatMapM)
import           Control.Monad.Reader (ReaderT (..), asks)
import           Control.Monad.Trans  (MonadTrans (..))
import           Data.List.Extra      (mconcatMap)
import           Data.Time.Format     (formatTime)
import           Hakyll
import           System.FilePath      (joinPath, (</>))

import           Config               (contentsRoot, defaultTimeLocale',
                                       siteName)
import           Config.Blog
import           Config.Contributions
import           Config.TopPage
import           Contexts             (siteCtx)
import qualified Contexts.Blog        as CtxBlog
import           Rules.Blog.Type
import           Rules.PageType
import           Utils                (mconcatM, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

lastUpdate :: (MonadMetadata m, MonadFail m) => [Item a] -> m String
lastUpdate [] = pure $ noPostsAlt topPageConfig
lastUpdate (x:_) = formatTime defaultTimeLocale' (postDateFormat topPageConfig)
    <$> getItemUTC defaultTimeLocale' (itemIdentifier x)

introDateCtx :: [Item a] -> BlogConfReader m Compiler (Context String)
introDateCtx posts = constField
    <$> asks ((<> "-intro-date") . blogName)
    <*> lift (lastUpdate posts)

listPostsCtx :: [Item String] -> BlogConfReader m Compiler (Context String)
listPostsCtx posts = do
    name <- asks ((<> "-posts") . blogName)
    pure $ listField name (siteCtx <> defaultContext) (pure posts)

mkBlogCtx :: BlogConfReader m Compiler (Context String)
mkBlogCtx = do
    ep <- asks blogEntryPattern
    cs <- asks blogContentSnapshot
    posts <- lift $ fmap (take $ maxTitleNum topPageConfig) . recentFirst =<< loadAllSnapshots ep cs
    mconcatM [
        listPostsCtx posts
      , CtxBlog.title
      , CtxBlog.description
      , introDateCtx posts
      , pure siteCtx
      , pure defaultContext
      ]

rules :: [BlogConfig m] -> PageConfReader Rules ()
rules bcs = do
    faIcons <- asks pcFaIcons
    projs <- lift $ preprocess renderProjectsList
    conts <- lift $ preprocess renderContributionsTable
    let baseCtx = mconcatMap (uncurry constField) [
            ("title", siteName)
          , ("projs", projs)
          , ("contable", conts)
          ]
    lift $ match indexPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
        compile $ do
            topCtx <- mappend baseCtx <$> mconcatMapM (runReaderT mkBlogCtx) bcs
            getResourceBody
                >>= applyAsTemplate topCtx
                >>= loadAndApplyTemplate rootTemplate topCtx
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
    where
        indexPath = fromGlob $ joinPath [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]


