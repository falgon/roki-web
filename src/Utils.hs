module Utils (
    absolutizeUrls,
    modifyExternalLinkAttr,
    sanitizeTagName,
    makePageIdentifier,
    getStringField,
    prependBaseUrl
) where

import Control.Monad (liftM2)
import Data.Char (toLower, isAlphaNum)
import Hakyll 
import System.FilePath ((</>), takeDirectory, takeFileName, normalise, isRelative, isAbsolute)
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
            | isExternalLink t = let (TS.TagOpen "a" as) = t in 
                TS.TagOpen "a" $ as <> extraAttributes
            | otherwise = t
        isExternalLink = liftM2 (&&) (TS.isTagOpenName "a") (isExternal . TS.fromAttrib "href")
        extraAttributes = [("target", "_blank"), ("rel", "nofollow noopener")]

sanitizeTagName :: String -> String
sanitizeTagName = map (\x -> if x == ' ' then '-' else toLower x) .
    filter (liftM2 (||) isAlphaNum (`elem` [' ', '-', '_']))

makePageIdentifier :: FilePath -> PageNumber -> Identifier
makePageIdentifier p 1 = fromFilePath p
makePageIdentifier p n = fromFilePath $ takeDirectory' p </> "page" </> show n </> takeFileName p
    where 
        takeDirectory' x = let x' = takeDirectory x in if x' == "." then "" else x'

getStringField :: String -> Context String -> Compiler (Maybe String)
getStringField key cs = do
    s <- unContext cs key [] (Item (fromFilePath "") "")
    return $ case s of
        StringField x -> Just x
        _ -> Nothing

prependBaseUrl :: String -> Item String -> Compiler (Item String)
prependBaseUrl base = return . fmap (withUrls prependBaseUrl')
    where
        prependBaseUrl' u
            | not (isExternal u) && isAbsolute u = base <> u
            | otherwise = u
