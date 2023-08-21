module Contexts.Blog (
    description
  , beforeContentBodyAdditionalComponent
  , headerAdditionalComponent
  , footerAdditionalComponent
) where

import           Hakyll

import           Config.Blog (BlogConfig (..))

description :: BlogConfig m -> Context String
description = constField "blog-description" . blogDescription

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
