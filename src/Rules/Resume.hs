{-# LANGUAGE OverloadedStrings #-}
module Rules.Resume (rules) where

import           Control.Monad.Reader  (asks)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.Functor          ((<&>))
import           Data.List             (intercalate, sortBy)
import           Data.Ord              (comparing)
import           Data.String           (IsString (..))
import           Data.Time.Calendar    (toGregorian)
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.LocalTime   (LocalTime (..), utcToLocalTime)
import           Hakyll
import           System.FilePath       (joinPath, (</>))
import           System.FilePath.Posix (takeBaseName)

import           Config                (contentsRoot, readerOptions, siteName,
                                        timeZoneJST)
import           Contexts              (siteCtx)
import           Rules.PageType
import           Utils                 (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome    as FA

resumeRoot :: FilePath
resumeRoot = joinPath [contentsRoot, "resume"]

aboutMeIdent :: Identifier
aboutMeIdent = fromString
    $ joinPath [resumeRoot, "about_me.md"]

resumeCareerPattern :: Pattern
resumeCareerPattern = fromRegex $ mconcat
    [ "(^"
    , joinPath [resumeRoot, "career", "[0-9]+\\.md"]
    , "$)"
    ]

skillsIdent :: Identifier
skillsIdent = fromString
    $ joinPath [resumeRoot, "skills.md"]

otherActivitiesIdent :: Identifier
otherActivitiesIdent = fromString
    $ joinPath [resumeRoot, "other_activities.md"]

favTechIdent :: Identifier
favTechIdent = fromString
    $ joinPath [resumeRoot, "fav_tech.md"]

sortByNum :: [Item a] -> [Item a]
sortByNum = sortBy
    $ flip
    $ comparing
    $ (read :: String -> Int) . takeBaseName . toFilePath . itemIdentifier

mdRule :: Snapshot
    -> Pattern
    -> PageConfReader Rules ()
mdRule ss pat = do
    wOpt <- asks pcWriterOpt
    katexRender <- asks pcKaTeXRender
    faIcons <- asks pcFaIcons
    lift $ match pat $ compile $ do
        pandocCompilerWith readerOptions wOpt
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons
            >>= katexRender
            >>= saveSnapshot ss

getCurrentDate :: IO String
getCurrentDate = getCurrentTime
    <&> toGregorian . localDay . utcToLocalTime timeZoneJST
    <&> \(y, m, d) -> intercalate "%2F" [show y, show m, show d]

rules :: PageConfReader Rules ()
rules = do
    let items = resumeCareerPattern : map (fromList . (:[]))
            [ aboutMeIdent
            , skillsIdent
            , otherActivitiesIdent
            , favTechIdent
            ]
    mapM_ (mdRule resumeSnapshot) items
    faIcons <- asks pcFaIcons
    lastUpdate <- lift $ preprocess getCurrentDate
    lift $ match resumeJPPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
        compile $ do
            am <- loadSnapshotBody aboutMeIdent resumeSnapshot
            career <- sortByNum <$> loadAllSnapshots resumeCareerPattern resumeSnapshot
            skills <- loadSnapshotBody skillsIdent resumeSnapshot
            oc <- loadSnapshotBody otherActivitiesIdent resumeSnapshot
            fav <- loadSnapshotBody favTechIdent resumeSnapshot
            let resumeCtx' = resumeCtx am career skills oc fav lastUpdate
            getResourceBody
                >>= applyAsTemplate resumeCtx'
                >>= loadAndApplyTemplate rootTemplate resumeCtx'
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
    where
        resumeSnapshot = "resumeSS"
        resumeCtx am career skills oc fav lastUpdate = mconcat [
            constField "title" $ "resume - " <> siteName
          , siteCtx
          , defaultContext
          , constField "about-me-body" am
          , listField "career-list" careerCtx $ pure career
          , constField "skills" skills
          , constField "other-activities-body" oc
          , constField "fav-tech" fav
          , constField "last-update" lastUpdate
          ]
        careerCtx = mconcat [
            metadataField
          , bodyField "career-body"
          ]
        resumeJPPath = fromGlob $ joinPath [contentsRoot, "pages", "resume", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]
