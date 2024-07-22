{-# LANGUAGE OverloadedStrings #-}
module Rules.Resume (rules) where

import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import           Data.String           (IsString (..))
import           Hakyll
import           System.FilePath       (joinPath, (</>))
import           System.FilePath.Posix (takeBaseName)
import           Text.Pandoc.Options   (WriterOptions)

import           Config                (contentsRoot, readerOptions, siteName)
import           Contexts              (siteCtx)
import           Utils                 (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome    as FA

resumeRoot :: FilePath
resumeRoot = joinPath [contentsRoot, "resume"]

aboutMeIdent :: Identifier
aboutMeIdent = fromString $ joinPath [resumeRoot, "about_me.md"]

resumeCareerPattern :: Pattern
resumeCareerPattern = fromRegex $ mconcat
    [ "(^"
    , joinPath [resumeRoot, "career", "[0-9]+\\.md"]
    , "$)"
    ]

otherActivitiesIdent :: Identifier
otherActivitiesIdent = fromString $ joinPath [resumeRoot, "other_activities.md"]

sortByNumber :: [Item a] -> [Item a]
sortByNumber = sortBy
    $ comparing
    $ (read :: String -> Int) . takeBaseName . toFilePath . itemIdentifier

mdRule :: Snapshot
    -> Pattern
    -> WriterOptions
    -> (Item String -> Compiler (Item String))
    -> FA.FontAwesomeIcons
    -> Rules ()
mdRule ss pat wOpt katexRender faIcons = match pat $ compile $ do
    pandocCompilerWith readerOptions wOpt
        >>= modifyExternalLinkAttr
        >>= relativizeUrls
        >>= FA.render faIcons
        >>= katexRender
        >>= saveSnapshot ss

rules :: WriterOptions -> (Item String -> Compiler (Item String)) -> FA.FontAwesomeIcons -> Rules ()
rules wOpt katexRender faIcons = do
    mdRule resumeSnapshot (fromList [aboutMeIdent]) wOpt katexRender faIcons
        *> mdRule resumeSnapshot resumeCareerPattern wOpt katexRender faIcons
        *> mdRule resumeSnapshot (fromList [otherActivitiesIdent]) wOpt katexRender faIcons
    match resumeJPPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
        compile $ do
            am <- loadSnapshotBody aboutMeIdent resumeSnapshot
            career <- sortByNumber <$> loadAllSnapshots resumeCareerPattern resumeSnapshot
            oc <- loadSnapshotBody otherActivitiesIdent resumeSnapshot
            let resumeCtx' = resumeCtx am career oc
            getResourceBody
                >>= applyAsTemplate resumeCtx'
                >>= loadAndApplyTemplate rootTemplate resumeCtx'
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
    where
        resumeSnapshot = "resumeSS"
        resumeCtx am career oc = mconcat [
            constField "title" $ "resume - " <> siteName
          , siteCtx
          , defaultContext
          , constField "about-me-body" am
          , constField "other-activities-body" oc
          , listField "career-list" careerCtx $ pure career
          ]
        careerCtx = mconcat [
            metadataField
          , bodyField "career-body"
          ]
        resumeJPPath = fromGlob $ joinPath [contentsRoot, "pages", "resume", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]
