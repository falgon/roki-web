{-# LANGUAGE OverloadedStrings #-}
module Rules.Resume (rules) where

import           Control.Monad         ((>=>))
import           Control.Monad.Extra   (concatMapM)
import           Control.Monad.Reader  (asks)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.Functor          ((<&>))
import           Data.List             (intercalate, sortBy)
import           Data.Ord              (comparing)
import           Data.String           (IsString (..))
import           Data.Time.Calendar    (toGregorian)
import           Data.Time.LocalTime   (LocalTime (..), utcToLocalTime)
import           Hakyll
import           System.FilePath       (joinPath, (</>))
import           System.FilePath.Posix (takeBaseName)

import           Config                (contentsRoot, readerOptions, siteName,
                                        timeZoneJST)
import           Contexts              (siteCtx)
import           Rules.PageType
import           Utils                 (mconcatM, modifyExternalLinkAttr)
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

getLastModDate :: [Pattern] -> Compiler String
getLastModDate items = getUnderlying
    >>= concatMapM (getMatches >=> mapM getItemModificationTime) . (:items) . fromList . (:[])
    <&> toGregorian . localDay . utcToLocalTime timeZoneJST . head . sortBy (flip compare)
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
    lift $ match resumeJPPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
        compile $ do
            resumeCtx <- mconcatM [
                pure $ constField "title" $ "resume - " <> siteName
              , pure siteCtx
              , pure defaultContext
              , constField "about-me-body"
                    <$> loadSnapshotBody aboutMeIdent resumeSnapshot
              , pure $ listField "career-list" (metadataField <> bodyField "career-body") $
                    sortByNum <$> loadAllSnapshots resumeCareerPattern resumeSnapshot
              , constField "skills"
                    <$> loadSnapshotBody skillsIdent resumeSnapshot
              , constField "other-activities-body"
                    <$> loadSnapshotBody otherActivitiesIdent resumeSnapshot
              , constField "fav-tech"
                    <$> loadSnapshotBody favTechIdent resumeSnapshot
              , constField "last-update"
                    <$> getLastModDate items
              ]
            getResourceBody
                >>= applyAsTemplate resumeCtx
                >>= loadAndApplyTemplate rootTemplate resumeCtx
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
    lift $ createRedirects [
        (fromString $ joinPath ["resume", "index.html"], joinPath ["resume", "jp.html"])
      ]
    where
        resumeSnapshot = "resumeSS"
        resumeJPPath = fromGlob $ joinPath [contentsRoot, "pages", "resume", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]
