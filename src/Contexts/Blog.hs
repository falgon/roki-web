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
) where

import           Control.Monad.Reader (asks, lift)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Hakyll
import           Lucid.Base           (Html, ToHtml (..), renderText, toHtml)
import           Lucid.Html5

import           Config.Blog          (BlogConfig (..))
import           Config.Site          (GSuite (..), gSuiteConf)
import           Rules.Blog.Type
import           Utils                (sanitizeDisqusName)

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
    => BlogConfReader m m (Context String)
gSuite = (<>)
    <$> asks (constField "google-cx" . ((gCxPrefix gSuiteConf <> ":") <>) . blogGoogleCx)
    <*> pure (constField "google-site-verification" (gSiteVerifyKey gSuiteConf))

disqus :: Monad m
    => BlogConfReader m m (Context String)
disqus = asks $ constField "disqus" . sanitizeDisqusName . blogName

