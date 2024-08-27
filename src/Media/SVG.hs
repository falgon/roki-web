{-# LANGUAGE LambdaCase #-}
module Media.SVG (
    optimizeSVGCompiler
  , mermaidTransform
) where

import           Control.Arrow             (first)
import           Control.Monad             (MonadPlus (..))
import           Control.Monad.Trans       (MonadTrans (..))
import           Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import qualified Data.Text                 as T
import           Hakyll
import           System.Exit               (ExitCode (..))
import           System.Process            (proc, readCreateProcessWithExitCode)
import           Text.Pandoc               (Block (..), Pandoc)
import           Text.Pandoc.Walk          (walkM)

optimizeSVGCompiler :: [String] -> Compiler (Item String)
optimizeSVGCompiler opts = getResourceString >>=
    withItemBody (unixFilter "npx" $ ["svgo", "-i", "-", "-o", "-"] ++ opts)

codeBlock :: Block -> Compiler Block
codeBlock cb@(CodeBlock attr@(_, _, t) contents) = runMaybeT codeBlock'
    >>= maybe (pure cb) pure
    where
        codeBlock' = do
            lang <- T.unpack . T.toLower <$> hoistMaybe (lookup "lang" $ map (first $ T.unpack . T.toLower) t)
            if lang /= "mermaid" then mzero else do
                lift (unsafeCompiler $ readCreateProcessWithExitCode (proc "npx" args) $ T.unpack contents) >>= \case
                    (ExitFailure _, _, err) -> lift $ fail err
                    (ExitSuccess, out, _) -> pure $ CodeBlock attr $ T.pack out
        args = ["mmdc", "-i", "/dev/stdin", "-e", "svg", "-o", "-"]
codeBlock x = pure x

mermaidTransform :: Pandoc -> Compiler Pandoc
mermaidTransform = walkM codeBlock
