{-# LANGUAGE OverloadedStrings #-}
module Contexts.Blog (
    title
  , font
  , description
  , description'
  , beforeContentBodyAdditionalComponent
  , headerAdditionalComponent
  , footerAdditionalComponent
  , tagCloud
  , gSuite
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

{-# INLINE toLink #-}
toLink :: String -> String -> Html ()
toLink text path = a_ [href_ (T.pack $ toUrl path)] $ span_ $ toHtml text

title :: Monad m => BlogConfReader n m (Context String)
title = constField "blog-title" <$> asks blogName

font :: Monad m => BlogConfReader n m (Context String)
font = constField "blog-font-html" <$> asks (show . blogFont)

description :: BlogConfig m -> Context String
description = constField "blog-description" . blogDescription

description' :: BlogConfReader m Compiler (Context String)
description' = constField "blog-description" <$> asks blogDescription

beforeContentBodyAdditionalComponent :: BlogConfig m -> Context String
beforeContentBodyAdditionalComponent = constField "before-content-body-additional-component"
    . show
    . blogBeforeContentBodyAdditional

headerAdditionalComponent :: BlogConfig m -> Context String
headerAdditionalComponent = constField "header-additional-component"
    . show
    . blogHeaderAdditional

footerAdditionalComponent :: BlogConfig m -> Context String
footerAdditionalComponent = constField "footer-additional-component"
    . show
    . blogFooterAdditional

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
