{-# LANGUAGE OverloadedStrings #-}
module Rules.Resume (rules) where

import           Hakyll
import           System.FilePath    (joinPath, (</>))

import           Config             (contentsRoot, siteName)
import           Contexts           (siteCtx)
import           Utils              (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome as FA

resumeCareerPattern :: Pattern
resumeCareerPattern = fromRegex $
    "(^"
    <> joinPath [contentsRoot, "resume", "career", ".+", "index\\.md"]
    <> "$)"

rules :: FA.FontAwesomeIcons -> Rules ()
rules faIcons = match resumeJPPath $ do
    route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
    compile $ do
        career <- loadAll resumeCareerPattern
        debugCompiler $ (<> " careers found") $ show $ length career
        getResourceBody
            >>= applyAsTemplate (resumeCtx career)
            >>= loadAndApplyTemplate rootTemplate (resumeCtx career)
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons
    where
        resumeCtx career = mconcat [
            constField "title" $ "resume - " <> siteName
          , siteCtx
          , defaultContext
          , listField "career-list" careerCtx $ pure career
          ]
        careerCtx = mconcat [
            metadataField
          , bodyField "career-body"
          ]
        resumeJPPath = fromGlob $ joinPath [contentsRoot, "pages", "resume", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]
