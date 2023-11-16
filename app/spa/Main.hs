{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Config                    (hakyllConfig)
import           Config.Site               (timeZoneJST)
import           Control.Applicative       (optional)
import           Control.Monad             (MonadPlus (..), unless, (>=>))
import           Control.Monad.Extra       (unlessM)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.String               (fromString)
import           Data.Time                 (UTCTime (..), ZonedTime (..),
                                            defaultTimeLocale, parseTimeM,
                                            zonedTimeToUTC)
import           Data.Time.Format          (formatTime)
import           Data.Tuple.Extra          (dupe, first, second)
import           Data.Version              (showVersion)
import           Development.GitRev        (gitBranch, gitHash)
import qualified Hakyll                    as H
import qualified Options.Applicative       as OA
import qualified Paths_roki_web            as P
import           System.Exit               (exitFailure, exitSuccess)
import           System.IO                 (hFlush, hPutStrLn, stderr, stdout)

putStrLnErrWithExit :: String -> IO ()
putStrLnErrWithExit = hPutStrLn stderr >=> const exitFailure

parseJSTTime :: String -> Maybe UTCTime
parseJSTTime = fmap (zonedTimeToUTC . flip ZonedTime timeZoneJST) .
    parseTimeM True defaultTimeLocale "%m-%d-%R"

cronize :: UTCTime -> String
cronize = formatTime defaultTimeLocale "%M %H %d %m *"

humanize :: UTCTime -> String
humanize = formatTime defaultTimeLocale "%m/%d %H:%M"

hakyllConfig' :: H.Configuration
hakyllConfig' = hakyllConfig {
    H.destinationDirectory = ".github/workflows/scheduled_post"
  , H.storeDirectory = ".scheduled_actions_cache"
  , H.tmpDirectory = ".scheduled_actions_cache/tmp"
  }

data Opts = Opts
    { optCmd            :: Command
    , optSchedulingDate :: Maybe String
    , optBranchName     :: Maybe String
    , optIsForceYes     :: Bool
    }

data Command = CmdCronExpr { work :: Opts -> IO () }
    | CmdGenYaml { work :: Opts -> IO () }
    | CmdClean { work :: Opts -> IO () }

showCexpr :: Opts -> IO ()
showCexpr opts = maybe (putStrLnErrWithExit "must be specified a valid date string") putStrLn $
    cronize <$> (parseJSTTime =<< optSchedulingDate opts)

execHakyllFromCmd :: H.Command -> H.Rules a -> IO ()
execHakyllFromCmd = H.hakyllWithArgs hakyllConfig' . H.Options False

execYaml' :: H.Command -> Opts -> MaybeT IO ()
execYaml' H.Build opts = let utc = parseJSTTime =<< optSchedulingDate opts in do
        fName <- MaybeT $ pure ((<> ".yml") <$> optBranchName opts)
        date <- MaybeT $ pure (humanize <$> utc)
        bName <- MaybeT $ pure (optBranchName opts)
        cExpr <- MaybeT $ pure (cronize <$> utc)
        lift $ do
            unless (optIsForceYes opts) $ do
                putStrLn $ "current branch name is: " <> $(gitBranch)
                putStr "Are you sure you want to continue? (y/N)" >> hFlush stdout
                unlessM (uncurry (||) . first (=='y') . second (=='Y') . dupe <$> getChar) $ putStrLn "Canceled" >> exitSuccess
            execHakyllFromCmd H.Build $ do
                H.create [H.fromFilePath fName] $ do
                    let ctx = H.constField "cron-expr" cExpr
                            <> H.constField "date" date
                            <> H.constField "branch-name" bName
                    H.route H.idRoute
                    H.compile $ H.makeItem ""
                        >>= H.loadAndApplyTemplate (H.fromFilePath "tools/scheduled_post/template.yml") ctx
                H.match (fromString "tools/scheduled_post/**") $ H.compile H.templateBodyCompiler
execYaml' H.Clean _ = lift $ execHakyllFromCmd H.Clean $ return ()
execYaml' _ _ = mzero

execYaml :: H.Command -> Opts -> IO ()
execYaml hcmd = runMaybeT . execYaml' hcmd >=> maybe (putStrLnErrWithExit "must be specified date and branch") return

cronExprCmd :: OA.Mod OA.CommandFields Command
cronExprCmd = OA.command "cexpr" $
    OA.info (pure $ CmdCronExpr showCexpr) $ OA.progDesc "show crontab expression"

genYamlCmd :: OA.Mod OA.CommandFields Command
genYamlCmd = OA.command "yaml" $
    OA.info (pure $ CmdGenYaml $ execYaml H.Build) $
        OA.progDesc "generate GitHub Actions yaml from template"

cleanCmd :: OA.Mod OA.CommandFields Command
cleanCmd = OA.command "clean" $
    OA.info (pure $ CmdClean $ execYaml H.Clean) $
        OA.progDesc "clean up and remove cache"

schedulingDate :: OA.Parser (Maybe String)
schedulingDate = optional $ OA.strOption $ mconcat [
    OA.metavar "date"
  , OA.short 'd'
  , OA.long "date"
  , OA.help "Date to schedule (mm-dd-%H:%M)"
  ]

branchName :: OA.Parser (Maybe String)
branchName = optional $ OA.strOption $ mconcat [
    OA.short 'b'
  , OA.long "branch-name"
  , OA.help "The name of the branch you plan to deploy"
  ]

forceYes :: OA.Parser Bool
forceYes = OA.switch $ mconcat [
    OA.short 'y'
  , OA.help "Generate a file without checking the branch name and repository name"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> OA.hsubparser (cronExprCmd <> genYamlCmd <> cleanCmd)
    <*> schedulingDate
    <*> branchName
    <*> forceYes

versionOption :: OA.Parser (a -> a)
versionOption = OA.infoOption vopt $ mconcat [
    OA.long "version"
  , OA.help "Show spa version information"
  ]
    where
        vopt = concat [
            "The roki-web Scheduling Post Action manager "
          , showVersion P.version
          , "\ncommit hash: "
          , $(gitHash)
          ]

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> versionOption <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc $ concat [
        "The roki-web Scheduling Post Action manager "
      , showVersion P.version
    ]
  ]

main :: IO ()
main = OA.execParser optsParser >>= uncurry work . first optCmd . dupe
