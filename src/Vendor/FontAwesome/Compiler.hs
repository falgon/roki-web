module Vendor.FontAwesome.Compiler (
    render
) where

import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (fromMaybe)
import           Hakyll
import qualified Text.HTML.TagSoup.Tree  as TT

import           Config                  (tagSoupOption)
import           Vendor.FontAwesome.Core

render :: FontAwesomeIcons -> Item String -> Compiler (Item String)
render icons = return . fmap
    (TT.renderTreeOptions tagSoupOption . TT.transformTree render' . TT.parseTree)
    where
        render' tag@(TT.TagBranch "i" as []) = case toFontAwesome $ classes as of
            Just tree -> [tree]
            Nothing   -> [tag]
        render' tag = [tag]

        toFontAwesome (p:('f':'a':'-':name):cs) = (`appendClasses` cs) <$> fontAwesome icons p name
        toFontAwesome _ = Nothing

        appendClasses t [] = t
        appendClasses (TT.TagBranch x y z) cs =
            let as1 = M.fromList y
                as2 = M.singleton "class" $ unwords cs
                y' = M.toList $ M.unionWith (\l r -> l <> " " <> r) as1 as2
            in TT.TagBranch x y' z
        appendClasses t _ = t

        classes = words . fromMaybe "" . lookup "class"
