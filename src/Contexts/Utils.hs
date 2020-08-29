module Contexts.Utils (
    metadataToListField
) where

import           Data.String     (fromString)
import           Hakyll
import           System.FilePath ((</>))

metadataToListField :: String -> String -> Context String
metadataToListField mdName mdKey = listFieldWith mdName ctx $ \item -> do
    mds <- getMetadataField (itemIdentifier item) mdName
    return $ case mds of
        Just xs -> map (itemize item . trim) $ splitAll "," xs
        Nothing -> []
    where
        ctx = field mdKey (return . itemBody) <> defaultContext
        itemize item md = Item {
            itemIdentifier = fromString (md <> "/" <> md)
          , itemBody = toFilePath (itemIdentifier item) </> md
        }
