{-# LANGUAGE OverloadedStrings #-}
module Rules.IndexPage (rules) where

import           Control.Monad        (forM)
import           Data.Time.Format     (formatTime)
import           Hakyll
import           System.FilePath      ((</>))

import           Config               (contentsRoot, defaultTimeLocale',
                                       siteName)
import           Config.Blog
import           Config.Contributions
import           Config.RegexUtils    (intercalateDir)
import           Contexts             (siteCtx)
import           Utils                (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

mkBlogCtx :: String -> BlogConfig m -> Compiler (Context String)
mkBlogCtx key obs = do
    posts <- fmap (take 4) . recentFirst =<< loadAllSnapshots (blogEntryPattern obs) (blogContentSnapshot obs)
    lastUpdate <- formatTime defaultTimeLocale' "%Y%%2F%m%%2F%d" -- "%%2F" is URL encoded slash
        <$> getItemUTC defaultTimeLocale' (itemIdentifier (head posts))
    return $ listField key (siteCtx <> defaultContext) (return posts)
        <> constField "blog-title" (blogName obs)
        <> constField "blog-description" (blogDescription obs)
        <> constField (blogName obs <> "-intro-date") lastUpdate
        <> siteCtx
        <> defaultContext

rules :: [BlogConfig m] -> FA.FontAwesomeIcons -> Rules ()
rules bcs faIcons = do
    projs <- preprocess renderProjectsList
    conts <- preprocess renderContributionsTable

    match indexPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const "")
        compile $ do
            blogs <- mconcat <$> forM bcs (\bc -> mkBlogCtx (blogName bc <> "-" <> "posts") bc)

            let aBlogCtx = constField "title" siteName
                    <> constField "projs" projs
                    <> constField "contable" conts
                    <> blogs

            getResourceBody
                >>= applyAsTemplate aBlogCtx
                >>= loadAndApplyTemplate rootTemplate aBlogCtx
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons

    match "CNAME" $ route idRoute >> compile copyFileCompiler
    match "ads.txt" $ route idRoute >> compile copyFileCompiler
    where
        indexPath = fromGlob $ intercalateDir [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ intercalateDir [contentsRoot, "templates", "site", "default.html"]

