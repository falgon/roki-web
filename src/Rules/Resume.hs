{-# LANGUAGE OverloadedStrings #-}
module Rules.Resume (rules) where

import           Hakyll
import           System.FilePath     (joinPath, (</>))
import           Text.Pandoc.Options (WriterOptions)

import           Config              (contentsRoot, readerOptions, siteName)
import           Contexts            (siteCtx)
import           Utils               (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome  as FA

resumeCareerPattern :: Pattern
resumeCareerPattern = fromRegex $ mconcat
    [ "(^"
    , joinPath [contentsRoot, "resume", "career", ".+", "index\\.md"]
    , "$)"
    ]

rules :: WriterOptions -> (Item String -> Compiler (Item String)) -> FA.FontAwesomeIcons -> Rules ()
rules wOpt katexRender faIcons = do
    match resumeCareerPattern $
        compile $ do
            pandocCompilerWith readerOptions wOpt
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
                >>= katexRender
                >>= saveSnapshot careerSnapshot

    match resumeJPPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
        compile $ do
            career <- chronological =<< loadAllSnapshots resumeCareerPattern careerSnapshot
            if null career then error $ show resumeCareerPattern else
                getResourceBody
                    >>= applyAsTemplate (resumeCtx career)
                    >>= loadAndApplyTemplate rootTemplate (resumeCtx career)
                    >>= modifyExternalLinkAttr
                    >>= relativizeUrls
                    >>= FA.render faIcons
    where
        careerSnapshot = "careerSS"
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
