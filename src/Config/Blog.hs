module Config.Blog (
    BlogConfig (..)
) where

import           Archives

import           Hakyll                hiding (FeedConfiguration (..),
                                        renderAtom)
import           Hakyll.Web.Feed.Extra (FeedConfiguration)
import           Lucid.Base            (Html)
import           Text.Pandoc.Options   (WriterOptions)

data BlogConfig m = BlogConfig {
    blogIsPreview                   :: Bool
  , blogName                        :: String
  , blogDescription                 :: String
  , blogFont                        :: Html ()
  , blogPageEntriesNum              :: Int
  , blogPrevNextTitleMaxNum         :: Int
  , blogFeedRecentNum               :: Int
  , blogHeaderAdditional            :: Html ()
  , blogBeforeContentBodyAdditional :: Html ()
  , blogFooterAdditional            :: Html ()
  , blogTagBuilder                  :: m Tags
  , blogTagPagesPath                :: FilePath -> FilePath
  , blogEntryPattern                :: Pattern
  , blogEntryFilesPattern           :: Pattern
  , blogFeedConfig                  :: FeedConfiguration
  , blogContentSnapshot             :: String
  , blogYearlyArchivesBuilder       :: m YearlyArchives
  , blogMonthlyArchivesBuilder      :: m MonthlyArchives
  , blogYearlyPagePath              :: FilePath -> FilePath
  , blogMonthlyPagePath             :: (FilePath, FilePath) -> FilePath
  , blogWriterOptions               :: WriterOptions
  , blogGoogleCx                    :: String
  , blogGiscusRepo                  :: String
  , blogGiscusRepoId                :: String
  , blogGiscusCategory              :: String
  , blogGiscusCategoryId            :: String
  }
