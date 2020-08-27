module Config.Blog (
    BlogConfig (..)
) where

import Archives
import Hakyll hiding (FeedConfiguration (..), renderAtom)
import Hakyll.Web.Feed.Extra (FeedConfiguration)
import Text.Pandoc.Options (WriterOptions)

data BlogConfig m = BlogConfig {
    blogName :: String
  , blogDescription :: String
  , blogFooterAdditional :: String
  , blogTagBuilder :: m Tags
  , blogTagPagesPath :: FilePath -> FilePath
  , blogEntryPattern :: Pattern
  , blogEntryFilesPattern :: Pattern
  , blogAtomConfig :: FeedConfiguration
  , blogContentSnapshot :: String
  , blogYearlyArchivesBuilder :: m YearlyArchives
  , blogMonthlyArchivesBuilder :: m MonthlyArchives
  , blogYearlyPagePath :: FilePath -> FilePath
  , blogMonthlyPagePath :: (FilePath, FilePath) -> FilePath
  , blogWriterOptions :: WriterOptions
  , blogGoogleCx :: String
  }
