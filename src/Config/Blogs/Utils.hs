{-# LANGUAGE OverloadedStrings #-}
module Config.Blogs.Utils (
    entryPattern
  , entryFilesPattern
  , contentSnapshot
  , tagPagesPath
  , buildTags
  , yearlyPagePath
  , buildYearlyArchives
  , monthlyPagePath
  , buildMonthlyArchives
) where

import qualified Hakyll            as H
import           System.FilePath   (joinPath, (</>))

import qualified Archives          as A
import           Config.Program    (contentsRoot)
import           Config.RegexUtils (dd, mm, yyyy)
import           Config.Site       (defaultTimeLocale', timeZoneJST)
import           Utils             (sanitizeTagName)

{-# INLINE postRoot #-}
postRoot :: String -> FilePath
postRoot blogName = joinPath [contentsRoot, blogName]

-- contents/roki.log/year/month/day/title/index.md
entryPattern :: String -> H.Pattern
entryPattern blogName = H.fromRegex $
    "(^"
    <> joinPath [postRoot blogName, yyyy, mm, dd, ".+", "index\\.md"]
    <> "$)"

entryFilesPattern :: String -> H.Pattern
entryFilesPattern blogName = H.fromRegex $
    "(^"
    <> joinPath [postRoot blogName, yyyy, mm, dd, ".+", ".+"]
    <> "$)"

{-# INLINE contentSnapshot #-}
contentSnapshot :: String -> H.Snapshot
contentSnapshot = (<> ".content")

{-# INLINE tagPagesPath #-}
tagPagesPath :: String -> FilePath -> FilePath
tagPagesPath blogName tag = joinPath [
    blogName
  , "tags"
  , sanitizeTagName tag
  , "index.html"
  ]

buildTags :: H.MonadMetadata m => String -> m H.Tags
buildTags blogName = H.buildTags (entryPattern blogName) $ H.fromFilePath . tagPagesPath blogName

{-# INLINE yearlyPagePath #-}
yearlyPagePath :: String -> FilePath -> FilePath
yearlyPagePath blogName year = joinPath [
    blogName
  , year
  , "index.html"
  ]

buildYearlyArchives :: (H.MonadMetadata m, MonadFail m) => String -> m A.YearlyArchives
buildYearlyArchives blogName = A.buildYearlyArchives defaultTimeLocale' timeZoneJST (entryPattern blogName) $
    H.fromFilePath . yearlyPagePath blogName

{-# INLINE monthlyPagePath #-}
monthlyPagePath :: String -> (FilePath, FilePath) -> FilePath
monthlyPagePath blogName (year, month) = joinPath [
    blogName
  , year
  , month
  , "index.html"
  ]

buildMonthlyArchives :: (H.MonadMetadata m, MonadFail m) => String -> m A.MonthlyArchives
buildMonthlyArchives blogName = A.buildMonthlyArchives defaultTimeLocale' timeZoneJST (entryPattern blogName) $
    H.fromFilePath . monthlyPagePath blogName

