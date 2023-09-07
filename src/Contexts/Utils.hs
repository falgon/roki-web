module Contexts.Utils (
    metadataToListField
) where

import           Data.Functor    ((<&>))
import           Data.String     (fromString)
import           Hakyll
import           System.FilePath ((</>))

metadataToListField :: String -> String -> Context String
metadataToListField mdName mdKey = listFieldWith mdName ctx $ \item ->
    getMetadataField (itemIdentifier item) mdName <&>
        maybe [] (map (itemize item . trim) . splitAll ",")
    where
        ctx = field mdKey (return . itemBody) <> defaultContext
        itemize item md = Item {
            itemIdentifier = fromString (md <> "/" <> md)
          , itemBody = toFilePath (itemIdentifier item) </> md
        }
