{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Media.SVG (
    optimizeSVGCompiler
  , mermaidTransform
) where

import           Control.Arrow             (first)
import           Control.Exception.Safe    (MonadThrow, throwString)
import           Control.Monad             (MonadPlus (..), (>=>))
import           Control.Monad.Extra       (ifM)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans       (MonadTrans (..))
import           Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import           Data.Functor              ((<&>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Hakyll
import           Lucid.Base                (HtmlT, renderText, toHtmlRaw)
import           Lucid.Html5
import           System.Exit               (ExitCode (..))
import           System.Process            (proc, readCreateProcessWithExitCode)
import           Text.Pandoc               (Block (..), Format (..),
                                            Inline (..), Pandoc)
import           Text.Pandoc.Walk          (walkM)

optimizeSVGCompiler :: [String] -> Compiler (Item String)
optimizeSVGCompiler opts = getResourceString
    >>= withItemBody (unixFilter "npx" $ ["svgo", "-i", "-", "-o", "-"] ++ opts)

execMmdc :: (MonadIO m, Monad n, MonadThrow m) => T.Text -> m (HtmlT n ())
execMmdc = liftIO . readCreateProcessWithExitCode (proc "npx" args) . T.unpack >=> \case
    (ExitFailure _, _, err) -> throwString err
    (ExitSuccess, out, _)   -> pure $ toHtmlRaw $ T.pack out
    where
        args = ["mmdc", "-i", "/dev/stdin", "-e", "svg", "-o", "-"]

mermaidCodeBlock :: Block -> Compiler Block
mermaidCodeBlock cb@(CodeBlock (_, _, t) contents) = maybe cb id <$>
    runMaybeT mermaidCodeBlock'
    where
        mermaidCodeBlock' =
            ifM ((/="mermaid") . T.toLower <$> hoistMaybe (lookup "lang" $ map (first $ T.unpack . T.toLower) t))
                mzero $
                lift $ unsafeCompiler (execMmdc contents)
                    <&> Plain . (:[]) . RawInline (Format "html")
                        . TL.toStrict . renderText
                        . div_ [class_ "has-text-centered"]
mermaidCodeBlock x = pure x

-- | When a code block starts in @```{lang=mermaid}@,
-- convert its internal mermaid format to svg.
mermaidTransform :: Pandoc -> Compiler Pandoc
mermaidTransform = walkM mermaidCodeBlock
