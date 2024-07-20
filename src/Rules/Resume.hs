{-# LANGUAGE OverloadedStrings #-}
module Rules.Resume (rules) where

import           Hakyll
import           System.FilePath    (joinPath, (</>))

import           Config             (contentsRoot, siteName)
import           Contexts           (siteCtx)
import           Utils              (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome as FA

rules :: FA.FontAwesomeIcons -> Rules ()
rules faIcons = match resumeJPPath $ do
    route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
    compile $ do
        getResourceBody
            >>= applyAsTemplate resumeCtx
            >>= loadAndApplyTemplate rootTemplate resumeCtx
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons
    where
        resumeCtx = mconcat [
            constField "title" $ "Resume - " <> siteName
          , siteCtx
          , defaultContext
          ]
        resumeJPPath = fromGlob $ joinPath [contentsRoot, "pages", "resume", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]

