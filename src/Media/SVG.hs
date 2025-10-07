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
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Hakyll
import           Lucid.Base                (HtmlT, renderText, toHtmlRaw)
import           Lucid.Html5
import           System.Exit               (ExitCode (..))
import           System.Process            (proc, readCreateProcessWithExitCode)
import           Text.Pandoc               (Block (..), Format (..),
                                            Inline (..))

optimizeSVGCompiler :: [String] -> Compiler (Item String)
optimizeSVGCompiler opts = getResourceString
    >>= withItemBody (unixFilter "npx" $ ["svgo", "-i", "-", "-o", "-"] ++ opts)

type SVGHtml n = HtmlT n ()

execMmdc :: (MonadIO m, Monad n, MonadThrow m) => T.Text -> m (SVGHtml n)
execMmdc = liftIO . readCreateProcessWithExitCode (proc "npx" args) . T.unpack >=> \case
    (ExitFailure _, _, err) -> throwString err
    (ExitSuccess, out, _)   -> pure $ toHtmlRaw $ T.pack out
    where
        args = ["mmdc", "-i", "/dev/stdin", "-e", "svg", "-o", "-"]

styledSvg :: Monad m => [(String, T.Text)] -> SVGHtml m -> SVGHtml m
styledSvg args svgHtml = figure_ [class_ "has-text-centered image"] $ do
    svgHtml
    maybe mempty (figcaption_ [class_ "has-text-centered"] . toHtmlRaw) $ lookup "caption" args

-- | When a code block starts in @```{lang=mermaid}@,
-- convert its internal mermaid format to svg.
-- Add the caption as follows: @```{lang=mermaid caption=hoge}@.
mermaidTransform :: Block -> Compiler Block
mermaidTransform cb@(CodeBlock (_, _, t) contents) = fromMaybe cb <$>
    runMaybeT mermaidTransform'
    where
        mermaidTransform' = let args = map (first $ T.unpack . T.toLower) t in
            ifM ((/="mermaid") . T.toLower <$> hoistMaybe (lookup "lang" args))
                mzero $
                lift $ unsafeCompiler (execMmdc contents)
                    <&> Plain . (:[]) . RawInline (Format "html")
                        . TL.toStrict . renderText . styledSvg args
mermaidTransform x = pure x
