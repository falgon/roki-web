{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Vendor.FontAwesome.Core (
    FontAwesomeIcons,
    fontAwesome,
    loadFontAwesome
) where

import           Data.Aeson                 (FromJSON (..), decode, withObject,
                                             (.!=), (.:), (.:?))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict        as M
import           System.Process             (readProcess)
import           Text.HTML.TagSoup          (Attribute)
import           Text.HTML.TagSoup.Tree     (TagTree (..))

data Elem = Elem { tag :: String, attr :: [Attribute String], child :: [Elem] } deriving Show

instance FromJSON Elem where
    parseJSON = withObject "Element" $ \obj -> do
        tag <- obj .: "tag"
        attr <- M.toList <$> obj .:? "attributes" .!= M.empty
        child <- obj .:? "children" .!= []
        return Elem {..}

type FontAwesomeIcons = M.HashMap String (M.HashMap String Elem)

loadFontAwesome :: IO (Maybe FontAwesomeIcons)
loadFontAwesome = decode . B.pack <$> readProcess "./tools/fontawesome.js" [] ""

fontAwesome :: FontAwesomeIcons -> String -> String -> Maybe (TagTree String)
fontAwesome db prefix name = toTagTree <$> (M.lookup prefix db >>= M.lookup name)

toTagTree :: Elem -> TagTree String
toTagTree = TagBranch <$> tag <*> attr <*> map toTagTree . child
