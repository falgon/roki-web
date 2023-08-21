module Contexts.Blog (
    title
  , description
  , description'
  , beforeContentBodyAdditionalComponent
  , headerAdditionalComponent
  , footerAdditionalComponent
) where

import           Control.Monad.Reader (asks)
import           Hakyll

import           Config.Blog          (BlogConfig (..))
import           Rules.Blog.Type

title :: BlogConfReader m Compiler (Context String)
title = constField "blog-title" <$> asks blogName

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
