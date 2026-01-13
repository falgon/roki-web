{-# LANGUAGE OverloadedStrings #-}
module Contexts.Blog (
    title
  , font
  , description
  , beforeContentBodyAdditionalComponent
  , headerAdditionalComponent
  , footerAdditionalComponent
  , tagCloud
  , gSuite
  , disqus
  , katexJsCtx
  , postCtx
  , listCtx
) where

import           Contexts.Field       (descriptionField, imageField,
                                       jsonLdArticleField, ogImageField,
                                       tagsField')
import           Control.Monad.Extra  (ifM)
import           Control.Monad.Reader (asks, lift)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Hakyll
import           Lucid.Base           (Html, ToHtml (..), renderText, toHtml)
import           Lucid.Html5

import           Config.Blog          (BlogConfig (..))
import           Config.Site          (GSuite (..), gSuiteConf)
import           Contexts.Core
import           Rules.Blog.Type
import           Utils                (mconcatM, sanitizeDisqusName)

{-# INLINE toLink #-}
toLink :: String -> String -> Html ()
toLink text path = a_ [href_ (T.pack $ toUrl path)] $ span_ $ toHtml text

title :: Monad m => BlogConfReader n m (Context String)
title = constField "blog-title" <$> asks blogName

font :: Monad m => BlogConfReader n m (Context String)
font = constField "blog-font-html" <$> asks (show . blogFont)

description :: Monad m => BlogConfReader n m (Context String)
description = constField "blog-description" <$> asks blogDescription

beforeContentBodyAdditionalComponent :: Monad m => BlogConfReader n m (Context String)
beforeContentBodyAdditionalComponent = constField "before-content-body-additional-component"
    <$> asks (show . blogBeforeContentBodyAdditional)

headerAdditionalComponent :: Monad m => BlogConfReader n m (Context String)
headerAdditionalComponent = constField "header-additional-component"
    <$> asks (show . blogHeaderAdditional)

footerAdditionalComponent :: Monad m => BlogConfReader n m (Context String)
footerAdditionalComponent = constField "footer-additional-component"
    <$> asks (show . blogFooterAdditional)

tagCloud :: Monad m
    => BlogConfReader m m (Context a)
tagCloud = do
    tags <- asks blogTagBuilder >>= lift
    pure $ field "tag-cloud" $ const $
        TL.unpack . renderText . div_ [class_ "tags"] . toHtmlRaw
            <$> renderTags toLink' concat tags
    where
        toLink' tag path = const $ const $ const $
            TL.unpack $ renderText $
                span_ [class_ "tag is-dark"] $ toLink tag path

gSuite :: Monad m
    => BlogConfReader n m (Context String)
gSuite = (<>)
    <$> asks (constField "google-cx" . ((gCxPrefix gSuiteConf <> ":") <>) . blogGoogleCx)
    <*> pure (constField "google-site-verification" (gSiteVerifyKey gSuiteConf))

disqus :: Monad m
    => BlogConfReader n m (Context String)
disqus = asks $ constField "disqus" . sanitizeDisqusName . blogName

katexJsCtx :: Monad m
    => BlogConfReader n m (Context String)
katexJsCtx = ifM (asks $ not . blogIsPreview) (pure mempty) $ pure $
    constField "katex-script" $ TL.unpack $ renderText $ do
        script_ [defer_ "", type_ "text/javascript", src_ "/vendor/katex/katex.min.js"] TL.empty
        script_ [defer_ "", type_ "text/javascript", src_ "/vendor/katex/auto-render.min.js"] TL.empty

postCtx :: Monad m
    => Tags
    -> BlogConfReader n m (Context String)
postCtx tags = do
    defaultOgImage <- asks $ \bc -> case blogName bc of
        "roki.log"   -> "/images/ogp/roki-log-default.png"
        "roki.diary" -> "/images/ogp/roki-diary-default.png"
        _            -> "/images/ogp/default.png"
    mconcatM [
        pure $ dateCtx
      , pure $ tagsField' "tags" tags
      , pure $ descriptionField "description" 150
      , pure $ imageField "image"
      , pure $ ogImageField "og-image" defaultOgImage
      , pure $ jsonLdArticleField "json-ld-article"
      , pure $ siteCtx
      , pure $ jsPathCtx
      , pure $ defaultContext
      , katexJsCtx
      ]

listCtx :: Monad m
    => BlogConfReader n m (Context String)
listCtx = mconcatM [
    pure $ siteCtx
  , pure $ bodyField "body"
  , pure $ metadataField
  , pure $ pathField "path"
  , pure $ urlField "url"
  , katexJsCtx
  ]
