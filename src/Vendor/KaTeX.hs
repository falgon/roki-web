{-# LANGUAGE LambdaCase #-}
module Vendor.KaTeX (
    KaTeXRender
  , render
) where

import           Control.Monad.Extra    (concatMapM)
import           Data.Maybe             (fromMaybe)
import           Hakyll
import qualified Text.HTML.TagSoup      as TS
import qualified Text.HTML.TagSoup.Tree as TT

import           Config                 (tagSoupOption)

type KaTeXRender = Item String -> Compiler (Item String)

transformTreeM :: Monad m => (TT.TagTree s -> m [TT.TagTree s]) -> [TT.TagTree s] -> m [TT.TagTree s]
transformTreeM act = concatMapM $ \case
    (TT.TagBranch x y z) -> transformTreeM act z >>= act . TT.TagBranch x y
    x                    -> act x

render :: KaTeXRender
render = withItemBody $ fmap (TT.renderTreeOptions tagSoupOption) . transformTreeM f . TT.parseTree
    where
        f tag@(TT.TagBranch _ as [TT.TagLeaf (TS.TagText e)])
            | hasMathClass as = TT.parseTree <$>
                unixFilter "tools/katex_runner.sh" ["displayMode" | hasDisplayClass as] e
            | otherwise = return [tag]
        f tag = return [tag]

        hasDisplayClass = elem "display" . classes
        hasMathClass = elem "math" . classes
        classes = words . fromMaybe "" . lookup "class"

