{-# LANGUAGE OverloadedStrings #-}
module Rules.TopPage (rules) where

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
import           Rules.Blog.Type
import           Utils                (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

lastUpdate :: (MonadMetadata m, MonadFail m) => [Item a] -> m String
lastUpdate [] = pure $ noPostsAlt topPageConfig
lastUpdate (x:_) = formatTime defaultTimeLocale' (postDateFormat topPageConfig)
    <$> getItemUTC defaultTimeLocale' (itemIdentifier x)

mkBlogCtx :: BlogConfReader m Compiler (Context String)
mkBlogCtx = do
    ep <- asks blogEntryPattern
    cs <- asks blogContentSnapshot
    posts <- lift $ fmap (take $ maxTitleNum topPageConfig) . recentFirst =<< loadAllSnapshots ep cs
    name <- asks blogName
    introDateCtx <- lift (constField (name <> "-intro-date") <$> lastUpdate posts)
    description <- asks blogDescription
    pure $ mconcat [
        listField (name <> "-posts") (siteCtx <> defaultContext) (pure posts)
      , constField "blog-title" name
      , constField "blog-description" description
      , introDateCtx
      , siteCtx
      , defaultContext
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

