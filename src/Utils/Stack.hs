{-# LANGUAGE LambdaCase #-}
module Utils.Stack (
    getStackYaml
  , getPackageConfig
  , getProgNameV
) where

import           Control.Arrow                ((&&&), (|||))
import           Data.Functor                 ((<&>))
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text                    as T
import           Data.Yaml                    (Value, decodeFileThrow)
import           Hpack.Config                 (DecodeResult (..), Package (..),
                                               defaultDecodeOptions,
                                               readPackageConfig)
import           RIO.Process                  (mkDefaultProcessContext)
import           Stack.Config                 (getProjectConfig)
import           Stack.Prelude                (Abs, File, LogLevel (LevelInfo),
                                               MonadIO, Path,
                                               StylesUpdate (StylesUpdate),
                                               newMVar, runRIO, throwString,
                                               toFilePath)
import           Stack.Types.GlobalOpts       (GlobalOpts (..))
import           Stack.Types.LockFileBehavior (LockFileBehavior (LFBReadOnly))
import           Stack.Types.ProjectConfig    (ProjectConfig (PCProject))
import           Stack.Types.Runner           (Runner (..))
import           Stack.Types.StackYamlLoc     (StackYamlLoc (SYLDefault))

gops :: GlobalOpts
gops = GlobalOpts {
    reExecVersion = Nothing
  , dockerEntrypoint = Nothing
  , logLevel = LevelInfo
  , timeInLog = False
  , rslInLog = False
  , planInLog = False
  , configMonoid = mempty
  , snapshot = Nothing
  , compiler = Nothing
  , terminal = False
  , stylesUpdate = StylesUpdate []
  , termWidthOpt = Nothing
  , stackYaml = SYLDefault
  , lockFileBehavior = LFBReadOnly
  }

mkRunner :: MonadIO m => m Runner
mkRunner = Runner gops False mempty 0
    <$> mkDefaultProcessContext
    <*> newMVar False

getStackYamlPath :: IO (Path Abs File)
getStackYamlPath = mkRunner
    >>= flip runRIO (getProjectConfig SYLDefault)
    >>= \case
        PCProject a -> pure a
        _           -> fail "Could not find stack location"

getStackYaml :: IO (HM.HashMap T.Text Value)
getStackYaml = getStackYamlPath
    >>= decodeFileThrow . toFilePath

getPackageConfig :: IO DecodeResult
getPackageConfig = readPackageConfig defaultDecodeOptions >>= throwString ||| pure

getProgNameV :: IO String
getProgNameV = getPackageConfig
    <&> uncurry (<>) . (packageName &&& (("/" <>) . packageVersion)) . decodeResultPackage
