{-# LANGUAGE OverloadedStrings #-}
module Rules.TopPage (rules) where

import           Control.Monad.Extra  (mconcatMapM)
import           Control.Monad.Reader (ReaderT (..), asks)
import           Control.Monad.Trans  (MonadTrans (..))
import           Data.List.Extra      (mconcatMap)
import           Data.Time.Format     (formatTime)
import           Hakyll
import           System.FilePath      (joinPath, (</>))
import           Text.Pandoc.Walk     (walkM)

import           Config               (contentsRoot, defaultTimeLocale',
                                       readerOptions, siteName)
import           Config.Blog
import           Config.Contributions
import           Config.TopPage
import           Contexts             (siteCtx)
import qualified Contexts.Blog        as CtxBlog
import           Media.SVG            (mermaidTransform)
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

aboutSnapshot :: Snapshot
aboutSnapshot = "aboutSS"

rules :: [BlogConfig m] -> PageConfReader Rules ()
rules bcs = do
    faIcons <- asks pcFaIcons
    wOpt <- asks pcWriterOpt
    projs <- lift $ preprocess renderProjectsList
    conts <- lift $ preprocess renderContributionsTable
    lift $ do
        match aboutPattern $ compile $
            pandocCompilerWithTransformM readerOptions wOpt (walkM mermaidTransform)
                >>= saveSnapshot aboutSnapshot
        match indexPath $ do
            route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
            compile $ do
                let baseCtx = mconcatMap (uncurry constField) [
                        ("title", siteName)
                      , ("projs", projs)
                      , ("contable", conts)
                      ]
                topCtx <- mconcatM [
                        pure baseCtx
                      , mconcatMapM (runReaderT mkBlogCtx) bcs
                      , constField "sitemap-body"
                            <$> loadSnapshotBody sitemapIdent aboutSnapshot
                      , constField "updates-body"
                            <$> loadSnapshotBody updatesIdent aboutSnapshot
                      ]
                getResourceBody
                    >>= applyAsTemplate topCtx
                    >>= loadAndApplyTemplate rootTemplate topCtx
                    >>= modifyExternalLinkAttr
                    >>= relativizeUrls
                    >>= FA.render faIcons
    where
        aboutPattern = fromGlob $ joinPath [contentsRoot, "about", "*.md"]
        sitemapIdent = fromFilePath $ joinPath [contentsRoot, "about", "sitemap.md"]
        updatesIdent = fromFilePath $ joinPath [contentsRoot, "about", "updates.md"]
        indexPath = fromGlob $ joinPath [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]


