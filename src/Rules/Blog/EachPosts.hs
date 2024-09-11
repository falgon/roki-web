module Rules.Blog.EachPosts (
    build
) where

import           Control.Monad.Reader       (asks)
import           Control.Monad.Trans        (MonadTrans (..))
import           Data.Bool                  (bool)
import           Hakyll
import           System.FilePath            ((</>))

import           Config                     (contentsRoot, readerOptions,
                                             tmBlogRoot)
import           Config.Blog                (BlogConfig (..))
import           Config.Site                (defaultTimeLocale', timeZoneJST)
import qualified Contexts.Blog              as BlogCtx
import           Media.SVG                  (mermaidTransform)
import           Rules.Blog.EachPosts.Utils
import           Rules.Blog.Type
import           Rules.Blog.Utils           (appendFooter)
import           Text.Pandoc.Walk           (walkM)
import           Utils                      (absolutizeUrls, mconcatM,
                                             modifyExternalLinkAttr)
import qualified Vendor.FontAwesome         as FA
import qualified Vendor.KaTeX               as KaTeX

build :: FA.FontAwesomeIcons
    -> Context String
    -> BlogConfReader m Rules Snapshot
build faIcons ctx = do
    disqusCtx <- mconcatM [
        pure ctx
      , BlogCtx.disqus
      ]
    wOptions <- asks blogWriterOptions
    cs <- asks blogContentSnapshot
    t <- asks blogName
    feedContent <- asks $ (<> "-feed-content") . blogName
    katexRender <- asks $ bool KaTeX.render pure . blogIsPreview

    eachPostsSeries $ \s -> do
        route $ gsubRoute (contentsRoot <> "/") (const mempty) `composeRoutes` setExtension "html"
        compile $ pandocCompilerWithTransformM readerOptions wOptions (walkM mermaidTransform)
            >>= absolutizeUrls
            >>= saveSnapshot feedContent
            >>= katexRender
            >>= saveSnapshot cs
            >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "post.html") (s <> disqusCtx)
            >>= appendFooter t defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "default.html") ctx
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons

    ep <- asks blogEntryFilesPattern
    lift $ match ep $ do
        route $ gsubRoute (contentsRoot <> "/") (const mempty)
        compile copyFileCompiler

    pure feedContent
