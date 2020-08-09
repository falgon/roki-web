module Rules.Blog.BlogConfig (
    BlogConfig (..)
) where

import Archives
import Hakyll
import Text.Pandoc.Options (WriterOptions)

data BlogConfig m = BlogConfig {
    blogName :: String
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
  , blogGoogleCx :: String
  , blogWriterOptions :: WriterOptions
  }
