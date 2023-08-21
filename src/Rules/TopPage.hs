{-# LANGUAGE OverloadedStrings #-}
module Rules.TopPage (rules) where

import           Data.Time.Format     (formatTime)
import           Hakyll
import           System.FilePath      (joinPath, (</>))

import           Config               (contentsRoot, defaultTimeLocale',
                                       siteName)
import           Config.Blog
import           Config.Contributions
import           Config.TopPage
import           Contexts             (siteCtx)
import           Utils                (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

lastUpdate :: (MonadMetadata m, MonadFail m) => [Item a] -> m String
lastUpdate [] = pure $ noPostsAlt topPageConfig
lastUpdate (x:_) = formatTime defaultTimeLocale' (postDateFormat topPageConfig)
    <$> getItemUTC defaultTimeLocale' (itemIdentifier x)

mkBlogCtx :: BlogConfig m -> Compiler (Context String)
mkBlogCtx obs = do
    posts <- fmap (take $ maxTitleNum topPageConfig) . recentFirst =<< loadAllSnapshots (blogEntryPattern obs) (blogContentSnapshot obs)
    introDateCtx <- constField (blogName obs <> "-intro-date") <$> lastUpdate posts
    pure $ mconcat [
        listField (blogName obs <> "-posts") (siteCtx <> defaultContext) (pure posts)
      , constField "blog-title" (blogName obs)
      , constField "blog-description" (blogDescription obs)
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
            blogs <- mconcat <$> mapM mkBlogCtx bcs
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

