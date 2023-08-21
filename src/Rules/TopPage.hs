{-# LANGUAGE OverloadedStrings #-}
module Rules.TopPage (rules) where

import           Control.Monad.Extra  (mconcatMapM)
import           Control.Monad.Reader (ReaderT (..), asks)
import           Control.Monad.Trans  (MonadTrans (..))
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
import           Utils                (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

lastUpdate :: (MonadMetadata m, MonadFail m) => [Item a] -> m String
lastUpdate [] = pure $ noPostsAlt topPageConfig
lastUpdate (x:_) = formatTime defaultTimeLocale' (postDateFormat topPageConfig)
    <$> getItemUTC defaultTimeLocale' (itemIdentifier x)

introDateCtx :: [Item a] -> BlogConfReader m Compiler (Context String)
introDateCtx posts = do
    name <- asks blogName
    constField (name <> "-intro-date") <$> lift (lastUpdate posts)

listPostsCtx :: [Item String] -> BlogConfReader m Compiler (Context String)
listPostsCtx posts = do
    name <- asks blogName
    pure $ listField (name <> "-posts") (siteCtx <> defaultContext) (pure posts)

mkBlogCtx :: BlogConfReader m Compiler (Context String)
mkBlogCtx = do
    ep <- asks blogEntryPattern
    cs <- asks blogContentSnapshot
    posts <- lift $ fmap (take $ maxTitleNum topPageConfig) . recentFirst =<< loadAllSnapshots ep cs
    mconcatMapM id [
        listPostsCtx posts
      , CtxBlog.title
      , CtxBlog.description'
      , introDateCtx posts
      , pure siteCtx
      , pure defaultContext
      ]

rules :: [BlogConfig m] -> FA.FontAwesomeIcons -> Rules ()
rules bcs faIcons = do
    projs <- preprocess renderProjectsList
    conts <- preprocess renderContributionsTable
    match indexPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
        compile $ do
            blogs <- mconcat <$> mapM (runReaderT mkBlogCtx) bcs
            let aBlogCtx = mconcat [
                    constField "title" siteName
                  , constField "projs" projs
                  , constField "contable" conts
                  , blogs
                  ]

            getResourceBody
                >>= applyAsTemplate aBlogCtx
                >>= loadAndApplyTemplate rootTemplate aBlogCtx
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
    where
        indexPath = fromGlob $ joinPath [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]

