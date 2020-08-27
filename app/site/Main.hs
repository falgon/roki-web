{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Data.Foldable (fold)
import qualified Data.Text.Lazy as TL
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Data.String (fromString)
import Hakyll
import qualified Paths_roki_web as P 
import qualified Options.Applicative as OA

import Config (hakyllConfig, siteName, writerOptions, writerPreviewOptions)
import Config.RegexUtils (intercalateDir)
import qualified Config.Blog as B
import qualified Config.Blogs.TechBlog as TB
import qualified Config.Blogs.AnotherBlog as AB
import qualified Vendor.FontAwesome as FA
import qualified Rules.Blog as B
import qualified Rules.Media as Media
import qualified Rules.Vendor as Vendor
import qualified Rules.Src.Style as Style
import qualified Rules.Src.JavaScript as Js
import qualified Rules.IndexPage as IP
import Lucid.Base (renderText)
import Lucid.Html5

data Opts = Opts 
    { optPreviewFlag :: !Bool
    , optVerbose :: !Bool
    , optInternalLinks :: !Bool
    , optCmd :: !Command
    }

{-# INLINE buildCmd #-}
buildCmd :: OA.Mod OA.CommandFields Command
buildCmd = OA.command "build" $ OA.info (pure Build) $ OA.progDesc "Generate the site"

{-# INLINE checkCmd #-}
checkCmd :: OA.Mod OA.CommandFields Command
checkCmd = OA.command "check" $ OA.info (pure $ Check False) $ OA.progDesc "Validate the site output"

{-# INLINE cleanCmd #-}
cleanCmd :: OA.Mod OA.CommandFields Command
cleanCmd = OA.command "clean" $ OA.info (pure Clean) $ OA.progDesc "Clean up and remove cache"

{-# INLINE deployCmd #-}
deployCmd :: OA.Mod OA.CommandFields Command
deployCmd = OA.command "deploy" $ OA.info (pure Deploy) $ OA.progDesc $ "Upload/deploy " <> siteName

{-# INLINE rebuildCmd #-}
rebuildCmd :: OA.Mod OA.CommandFields Command
rebuildCmd = OA.command "rebuild" $ OA.info (pure Rebuild) $ OA.progDesc "Clean and build again"

{-# INLINE serverCmd #-}
serverCmd :: Configuration -> OA.Mod OA.CommandFields Command
serverCmd conf = OA.command "server" $ 
    OA.info (pure $ Server (previewHost conf) (previewPort conf)) $ 
        OA.progDesc "Start a preview server"

{-# INLINE watchCmd #-}
watchCmd :: Configuration -> OA.Mod OA.CommandFields Command
watchCmd conf = OA.command "watch" $
    OA.info (pure $ Watch (previewHost conf) (previewPort conf) False) $ 
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

programOptions :: Configuration -> OA.Parser Opts
programOptions conf = Opts 
    <$> preview 
    <*> verbose
    <*> internalLinks
    <*> OA.hsubparser (buildCmd 
        <> checkCmd 
        <> cleanCmd 
        <> deployCmd 
        <> rebuildCmd 
        <> serverCmd conf
        <> watchCmd conf)

versionOption :: OA.Parser (a -> a)
versionOption = OA.infoOption vopt $ mconcat [OA.long "version", OA.help "Show version"]
    where
        vopt = concat [
            "The static site roki.dev compiler\nversion: "
          , showVersion P.version
          , ", commit hash: "
          , $(gitHash)
          ]

optsParser :: Configuration -> OA.ParserInfo Opts
optsParser conf = OA.info (OA.helper <*> versionOption <*> programOptions conf) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ concat [
        "The static site roki.dev compiler version "
      , showVersion P.version
      , " powerted by Hakyll"
      ]
  ]


haskellJp :: String
haskellJp = TL.unpack $ renderText $ do
    a_ [href_ "https://haskell.jp/blog/posts/links.html#roki.dev/roki.log/"] $
        img_ [
            width_ "234"
          , class_ "mt-1"
          , src_ "https://haskell.jp/img/supported-by-haskell-jp.svg"
          , alt_"Supported By Haskell-jp."
          ]

techBlogConf :: B.BlogConfig Rules
techBlogConf = B.BlogConfig {
    B.blogName = TB.blogName
  , B.blogDescription = TB.blogDesc
  , B.blogFooterAdditional = haskellJp
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
  , B.blogFooterAdditional = mempty
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
    opts <- OA.execParser $ optsParser hakyllConfig
    hakyllWithArgs hakyllConfig (Options (optVerbose opts) $ mapIL (optInternalLinks opts) (optCmd opts)) $ do
        Media.rules >> Vendor.rules (optPreviewFlag opts) >> Style.rules >> Js.rules
        faIcons <- fold <$> preprocess FA.loadFontAwesome

        let writer = if optPreviewFlag opts then writerPreviewOptions else writerOptions
            tc = techBlogConf { B.blogWriterOptions = writer }
            dc = diaryConf { B.blogWriterOptions = writer }

        B.blogRules (optPreviewFlag opts) tc faIcons
        B.blogRules (optPreviewFlag opts) dc faIcons
        IP.rules [tc, dc] faIcons

        match (fromString $ intercalateDir ["contents", "templates", "**"]) $ 
            compile templateBodyCompiler
    where
        mapIL b (Check _) = Check b
        mapIL _ x = x
