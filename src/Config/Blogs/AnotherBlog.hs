{-# LANGUAGE OverloadedStrings #-}
module Config.Blogs.AnotherBlog (
    blogName
  , blogDesc
  , entryPattern
  , entryFilesPattern
  , feedConfig
  , contentSnapshot
  , tagPagesPath
  , buildTags
  , yearlyPagePath
  , buildYearlyArchives
  , monthlyPagePath
  , buildMonthlyArchives
) where

import           Data.String           (fromString)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Hakyll                as H
import           Hakyll.Web.Feed.Extra hiding (renderAtom)
import           Lucid.Base            (renderText)
import           Lucid.Html5

import qualified Archives              as A
import qualified Config.Blogs.Utils    as BU
import           Config.Site           (siteName)

{-# INLINE blogName #-}
blogName :: FilePath
blogName = "roki.diary"

blogDesc :: String
blogDesc = TL.unpack $ renderText $ do
    a_ [href_ $ T.pack $ "/" <> blogName] $ fromString blogName
    p_ [class_ "is-inline"] $
        " is just a diary. I write about my daily events and thoughts "
            <> "(Most of the content of the article is written in Japanese)."

-- contents/roki.log/year/month/day/title/index.md
entryPattern :: H.Pattern
entryPattern = BU.entryPattern blogName

entryFilesPattern :: H.Pattern
entryFilesPattern = BU.entryFilesPattern blogName

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration {
    feedTitle = blogName
  , feedWebRoot = "https://" <> siteName
  , feedBlogName = blogName
  , feedDescription = "Roki Diary"
  , feedAuthorName = "Roki"
  , feedAuthorEmail = "falgon53@yahoo.co.jp"
  }

{-# INLINE contentSnapshot #-}
contentSnapshot :: H.Snapshot
contentSnapshot = BU.contentSnapshot blogName

tagPagesPath :: FilePath -> FilePath
tagPagesPath = BU.tagPagesPath blogName

buildTags :: H.MonadMetadata m => m H.Tags
buildTags = BU.buildTags blogName

{-# INLINE yearlyPagePath #-}
yearlyPagePath :: FilePath -> FilePath
yearlyPagePath = BU.yearlyPagePath blogName

buildYearlyArchives :: (H.MonadMetadata m, MonadFail m) => m A.YearlyArchives
buildYearlyArchives = BU.buildYearlyArchives blogName

{-# INLINE monthlyPagePath #-}
monthlyPagePath :: (FilePath, FilePath) -> FilePath
monthlyPagePath = BU.monthlyPagePath blogName

buildMonthlyArchives :: (H.MonadMetadata m, MonadFail m) => m A.MonthlyArchives
buildMonthlyArchives = BU.buildMonthlyArchives blogName

