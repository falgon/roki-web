{-# LANGUAGE OverloadedStrings #-}
module Rules.IndexPage (rules) where

import           Control.Monad        (forM)
import           Hakyll
import           System.FilePath      ((</>))

import           Config               (contentsRoot, siteName)
import           Config.Blog
import           Config.Contributions
import           Config.RegexUtils    (intercalateDir)
import           Contexts             (siteCtx)
import           Utils                (absolutizeUrls, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

mkBlogCtx :: String -> BlogConfig m -> Compiler (Context String)
mkBlogCtx key obs = do
    posts <- fmap (take 4) . recentFirst =<< loadAllSnapshots (blogEntryPattern obs) (blogContentSnapshot obs)
    return $ listField key (siteCtx <> defaultContext) (return posts)
        <> constField "blog-title" (blogName obs)
        <> constField "blog-description" (blogDescription obs)
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
                >>= absolutizeUrls
                >>= applyAsTemplate aBlogCtx
                >>= loadAndApplyTemplate rootTemplate aBlogCtx
                >>= modifyExternalLinkAttr
                >>= FA.render faIcons

    match "CNAME" $ route idRoute >> compile copyFileCompiler
    where
        indexPath = fromGlob $ intercalateDir [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ intercalateDir [contentsRoot, "templates", "site", "default.html"]

