module Vendor.FontAwesome.Compiler (
    render
  , renderWithSprite
) where

import           Control.Monad.State.Strict (State, modify', runState)
import qualified Data.HashMap.Strict        as M
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Hakyll
import qualified Text.HTML.TagSoup.Tree     as TT

import           Config                     (tagSoupOption)
import           Vendor.FontAwesome.Core

render :: FontAwesomeIcons -> Item String -> Compiler (Item String)
render icons = return . fmap
    (TT.renderTreeOptions tagSoupOption . TT.transformTree render' . TT.parseTree)
  where
    render' tag = [fromMaybe tag (renderIcon icons tag)]

renderWithSprite :: FontAwesomeIcons -> Item String -> Compiler (Item String)
renderWithSprite icons = return . fmap
    (TT.renderTreeOptions tagSoupOption . TT.transformTree renderBody . TT.parseTree)
  where
    renderBody (TT.TagBranch "body" attrs children) =
        let (rendered, symbols) = runState (mapM renderTag children) Map.empty
            sprite = TT.TagBranch "svg" [("style", "display:none"), ("aria-hidden", "true")]
                (Map.elems symbols)
        in [TT.TagBranch "body" attrs (if Map.null symbols then rendered else sprite : rendered)]
    renderBody tag = [tag]

    renderTag :: TT.TagTree String -> State (Map.Map String (TT.TagTree String)) (TT.TagTree String)
    renderTag tag = case renderIcon icons tag of
        Just (TT.TagBranch "svg" attrs paths)
            | Just prefix <- lookup "data-prefix" attrs
            , Just name <- lookup "data-icon" attrs
            , Just viewBox <- lookup "viewBox" attrs
            , lookup "aria-hidden" attrs == Just "true" -> do
                let symbolId = "fa-" <> prefix <> "-" <> name
                    symbol = TT.TagBranch "symbol" [("id", symbolId), ("viewBox", viewBox)] paths
                    use = TT.TagBranch "use" [("href", '#' : symbolId)] []
                modify' (Map.insert symbolId symbol)
                pure $ TT.TagBranch "svg" (filter ((/= "role") . fst) attrs) [use]
        Just svg -> pure svg
        Nothing  -> case tag of
            TT.TagBranch name attrs children -> TT.TagBranch name attrs <$> mapM renderTag children
            _                                -> pure tag

renderIcon :: FontAwesomeIcons -> TT.TagTree String -> Maybe (TT.TagTree String)
renderIcon icons (TT.TagBranch "i" attrs []) = case classes attrs of
    (prefix:('f':'a':'-':name):cs) -> (`appendClasses` cs) <$> fontAwesome icons prefix name
    _                             -> Nothing
  where
    appendClasses t [] = t
    appendClasses (TT.TagBranch x y z) cs =
        let as1 = M.fromList y
            as2 = M.singleton "class" $ unwords cs
            y' = M.toList $ M.unionWith (\l r -> l <> " " <> r) as1 as2
        in TT.TagBranch x y' z
    appendClasses t _ = t

    classes = words . fromMaybe "" . lookup "class"
renderIcon _ _ = Nothing
