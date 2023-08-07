{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import           Data.Foldable            (fold)
import           Data.String              (fromString)
import qualified Data.Text.Lazy           as TL
import           Data.Version             (showVersion)
import           Development.GitRev       (gitHash)
import           Hakyll
import qualified Options.Applicative      as OA
import qualified Paths_roki_web           as P

import           Config                   (hakyllConfig, siteName,
                                           writerOptions, writerPreviewOptions)
import qualified Config.Blog              as B
import qualified Config.Blogs.AnotherBlog as AB
import qualified Config.Blogs.TechBlog    as TB
import           Config.RegexUtils        (intercalateDir)
import qualified Contexts.Field.RokiDiary as RokiDiary
import qualified Contexts.Field.RokiLog   as RokiLog
import           Lucid.Base               (renderText)
import qualified Rules.Blog               as B
import qualified Rules.IndexPage          as IP
import qualified Rules.Media              as Media
import qualified Rules.Src.JavaScript     as Js
import qualified Rules.Src.Style          as Style
import qualified Rules.Vendor             as Vendor
import qualified Vendor.FontAwesome       as FA

data Opts = Opts
    { optPreviewFlag   :: !Bool
    , optVerbose       :: !Bool
    , optInternalLinks :: !Bool
    , optHost          :: String
    , optPort          :: !Int
    , optCmd           :: Configuration -> Command
    }

{-# INLINE buildCmd #-}
buildCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
buildCmd = OA.command "build"
    $ OA.info (pure $ const Build)
    $ OA.progDesc "Generate the site"

{-# INLINE checkCmd #-}
checkCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
checkCmd = OA.command "check"
    $ OA.info (pure $ const $ Check False)
    $ OA.progDesc "Validate the site output"

{-# INLINE cleanCmd #-}
cleanCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
cleanCmd = OA.command "clean"
    $ OA.info (pure $ const Clean)
    $ OA.progDesc "Clean up and remove cache"

{-# INLINE deployCmd #-}
deployCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
deployCmd = OA.command "deploy"
    $ OA.info (pure $ const Deploy)
    $ OA.progDesc $ "Upload/deploy " <> siteName

{-# INLINE rebuildCmd #-}
rebuildCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
rebuildCmd = OA.command "rebuild"
    $ OA.info (pure $ const Rebuild)
    $ OA.progDesc "Clean and build again"

{-# INLINE serverCmd #-}
serverCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
serverCmd = OA.command "server" $
    OA.info (pure $ \conf -> Server (previewHost conf) (previewPort conf)) $
        OA.progDesc "Start a preview server"

{-# INLINE watchCmd #-}
watchCmd :: OA.Mod OA.CommandFields (Configuration -> Command)
watchCmd = OA.command "watch" $
    OA.info (pure $ \conf -> Watch (previewHost conf) (previewPort conf) False) $
        OA.progDesc "Autocompile on changes and start a preview server"

preview :: OA.Parser Bool
preview = OA.switch $ mconcat [
    OA.long "preview"
  , OA.help "Enable a preview flag"
  ]

verbose :: OA.Parser Bool
verbose = OA.switch $ mconcat [
    OA.long "verbose"
  , OA.short 'v'
  , OA.help "Run in verbose mode"
  ]

internalLinks :: OA.Parser Bool
internalLinks = OA.switch $ mconcat [
    OA.long "internal-links"
  , OA.help "Check internal links only"
  ]

hostName :: OA.Parser String
hostName = OA.option OA.str $ mconcat [
    OA.long "host"
  , OA.short 'h'
  , OA.value "127.0.0.1"
  , OA.help "Host name"
  , OA.metavar "<host name>"
  ]

portNum :: OA.Parser Int
portNum = OA.option OA.auto $ mconcat [
    OA.long "port"
  , OA.short 'p'
  , OA.value 8888
  , OA.help "Port number"
  , OA.metavar "<port number (0-65535)>"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> preview
    <*> verbose
    <*> internalLinks
    <*> hostName
    <*> portNum
    <*> OA.hsubparser (mconcat [
        buildCmd
      , checkCmd
      , cleanCmd
      , deployCmd
      , rebuildCmd
      , serverCmd
      , watchCmd
      ])

versionOption :: OA.Parser (a -> a)
versionOption = OA.infoOption vopt $ mconcat [OA.long "version", OA.help "Show version"]
    where
        vopt = concat [
            "The static site roki.dev compiler\nversion: "
          , showVersion P.version
          , ", commit hash: "
          , $(gitHash)
          ]

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> versionOption <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ concat [
        "The static site roki.dev compiler version "
      , showVersion P.version
      , " powerted by Hakyll"
      ]
  ]

techBlogConf :: B.BlogConfig Rules
techBlogConf = B.BlogConfig {
    B.blogName = TB.blogName
  , B.blogDescription = TB.blogDesc
  , B.blogFont = RokiLog.font
  , B.blogHeaderAdditional = mempty
  , B.blogBeforeContentBodyAdditional = TL.unpack $ renderText RokiLog.gAdSenseBeforeContentBody
  , B.blogFooterAdditional = TL.unpack $ renderText RokiLog.footerAdditionalComponent
  , B.blogTagBuilder = TB.buildTags
  , B.blogTagPagesPath = TB.tagPagesPath
  , B.blogEntryPattern = TB.entryPattern
  , B.blogEntryFilesPattern = TB.entryFilesPattern
  , B.blogAtomConfig = TB.atomConfig
  , B.blogContentSnapshot = TB.contentSnapshot
  , B.blogYearlyArchivesBuilder = TB.buildYearlyArchives
  , B.blogMonthlyArchivesBuilder = TB.buildMonthlyArchives
  , B.blogYearlyPagePath = TB.yearlyPagePath
  , B.blogMonthlyPagePath = TB.monthlyPagePath
  , B.blogWriterOptions = writerOptions
  , B.blogGoogleCx = "c0ipiy0rxaw"
  }

diaryConf :: B.BlogConfig Rules
diaryConf = B.BlogConfig {
    B.blogName = AB.blogName
  , B.blogDescription = AB.blogDesc
  , B.blogFont = RokiDiary.font
  , B.blogHeaderAdditional = TL.unpack $ renderText RokiDiary.gAdSenseHeader
  , B.blogBeforeContentBodyAdditional = TL.unpack $ renderText RokiDiary.gAdSenseBeforeContentBody
  , B.blogFooterAdditional = TL.unpack $ renderText RokiDiary.gAdSenseFooter
  , B.blogTagBuilder = AB.buildTags
  , B.blogTagPagesPath = AB.tagPagesPath
  , B.blogEntryPattern = AB.entryPattern
  , B.blogEntryFilesPattern = AB.entryFilesPattern
  , B.blogAtomConfig = AB.atomConfig
  , B.blogContentSnapshot = AB.contentSnapshot
  , B.blogYearlyArchivesBuilder = AB.buildYearlyArchives
  , B.blogMonthlyArchivesBuilder = AB.buildMonthlyArchives
  , B.blogYearlyPagePath = AB.yearlyPagePath
  , B.blogMonthlyPagePath = AB.monthlyPagePath
  , B.blogWriterOptions = writerOptions
  , B.blogGoogleCx = "rzk_3jogdf4"
  }

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    let conf = hakyllConfig {
        previewHost = optHost opts
      , previewPort = optPort opts
      }
        writer = if optPreviewFlag opts then writerPreviewOptions else writerOptions
        blogConfs = [
            techBlogConf { B.blogWriterOptions = writer }
          , diaryConf { B.blogWriterOptions = writer }
          ]

    hakyllWithArgs conf (Options (optVerbose opts) $ mapIL (optInternalLinks opts) $ optCmd opts $ conf) $ do
        Media.rules
            *> Vendor.rules (optPreviewFlag opts)
            *> Style.rules
            *> Js.rules
        faIcons <- fold <$> preprocess FA.loadFontAwesome
        mapM_ (flip (B.blogRules (optPreviewFlag opts)) faIcons) blogConfs
        IP.rules blogConfs faIcons

        match (fromString $ intercalateDir ["contents", "templates", "**"]) $
            compile templateBodyCompiler
    where
        mapIL b (Check _) = Check b
        mapIL _ x         = x
