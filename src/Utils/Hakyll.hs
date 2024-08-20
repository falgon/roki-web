{-# LANGUAGE LambdaCase #-}
module Utils.Hakyll (
    absolutizeUrls
  , modifyExternalLinkAttr
  , sanitizeTagName
  , sanitizeDisqusName
  , makePageIdentifier
  , getStringField
) where

import           Control.Monad     (liftM2)
import           Data.Char         (isAlphaNum, isSpace, toLower)
import           Hakyll
import           System.FilePath   (isRelative, normalise, takeDirectory,
                                    takeFileName, (</>))
import qualified Text.HTML.TagSoup as TS

absolutizeUrls :: Item String -> Compiler (Item String)
absolutizeUrls item = getUnderlying >>= fmap (maybe item (flip fmap item . withUrls . f)) . getRoute
    where
        f r u
            | not (isExternal u) && isRelative u = normalise $ "/" </> takeDirectory r </> u
            | otherwise = u

modifyExternalLinkAttr :: Item String -> Compiler (Item String)
modifyExternalLinkAttr = return . fmap (withTags f)
    where
        f t
            | isExternalLink t = case t of
                (TS.TagOpen "a" as) -> TS.TagOpen "a" $ as <> extraAttributes
                _                   -> t
            | otherwise = t
        isExternalLink = liftM2 (&&) (TS.isTagOpenName "a") (isExternal . TS.fromAttrib "href")
        extraAttributes = [("target", "_blank"), ("rel", "nofollow noopener")]

sanitizeTagName :: String -> String
sanitizeTagName = map (\x -> if isSpace x then '-' else toLower x) .
    filter (liftM2 (||) isSpace (liftM2 (||) isAlphaNum (`elem` ['-', '_'])))

makePageIdentifier :: FilePath -> PageNumber -> Identifier
makePageIdentifier p 1 = fromFilePath p
makePageIdentifier p n = fromFilePath $ takeDirectory' p </> "page" </> show n </> takeFileName p
    where
        takeDirectory' x = let x' = takeDirectory x in if x' == "." then mempty else x'

getStringField :: String -> Context String -> Compiler (Maybe String)
getStringField key cs = unContext cs key mempty (Item (fromFilePath mempty) mempty) >>= \case
    StringField x -> pure $ Just x
    _             -> pure Nothing

sanitizeDisqusName :: String -> String
sanitizeDisqusName = map (\x -> if x == '.' then '-' else x)

