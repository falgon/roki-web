{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field (
    localDateField
  , tagsField'
  , tagCloudField'
  , descriptionField
  , imageField
  , yearMonthArchiveField
  , searchBoxResultField
  , haskellJpLogo
  , gAdSenseHeader
  , gAdSenseBody
) where

import           Control.Monad       (forM_, liftM2)
import           Control.Monad.Trans (lift)
import           Data.Function       (on)
import           Data.Functor        ((<&>))
import           Data.List           (isSuffixOf, sortBy)
import           Data.Maybe          (catMaybes, fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import           Data.Time.Format    (TimeLocale (..), formatTime)
import           Data.Time.LocalTime (TimeZone (..), utcToLocalTime)
import           Hakyll
import           Lucid.Base          (Html, ToHtml (..), renderText,
                                      renderTextT, toHtml)
import           Lucid.Html5
import qualified Text.HTML.TagSoup   as TS

import           Archives            (Archives (..), MonthlyArchives,
                                      YearlyArchives)

toLink :: String -> String -> Html ()
toLink text path = a_ [href_ (T.pack $ toUrl path)] $ span_ $ toHtml text

localDateField :: TimeLocale -> TimeZone -> String -> String -> Context a
localDateField locale zone key format = field key $
    fmap (formatTime locale format . utcToLocalTime zone) . getItemUTC locale . itemIdentifier

imageField :: String -> Context String
imageField key = field key $ \item ->
    case extractImages $ TS.parseTags $ itemBody item of
        [] -> noResult ("Field " ++ key ++ ": " ++ show (itemIdentifier item) ++ "has no image")
        (src:_) -> return src
    where
        extractImages = map (TS.fromAttrib "src") . filter f
        f tag =
            let src = TS.fromAttrib "src" tag
                cond = not $ null src || isExternal src || ".svg" `isSuffixOf` src
            in TS.isTagOpenName "img" tag && cond

descriptionField :: String -> Int -> Context String
descriptionField key len = field key $ const $
    take len . escapeHtml . concat . lines . itemBody <$> getResourceBody

tagsField' :: String -> Tags -> Context a
tagsField' key tags = field key $ \item -> do
    links <- getTags (itemIdentifier item)
        >>= mapM (liftM2 (<$>) toLink' (getRoute . tagsMakeId tags))
        <&> catMaybes
    if null links
        then noResult ("Field " ++ key ++ ": tag not set (" ++ show (itemIdentifier item) ++ ")")
        else return $ TL.unpack $ renderText $ mconcat $ map (span_ [class_ "tag is-dark"]) links
    where
        toLink' tag = fmap (toLink tag)

tagCloudField' :: String -> Tags -> Context a
tagCloudField' key tags = field key $ const $
    TL.unpack . renderText . div_ [class_ "tags"] . toHtmlRaw <$> renderTags toLink' concat tags
    where
        toLink' tag path = const $ const $ const $
            TL.unpack $ renderText $ span_ [class_ "tag is-dark"] $ toLink tag path


{-# INLINE buildYearMonthArchiveField #-}
buildYearMonthArchiveField :: YearlyArchives -> MonthlyArchives -> Maybe String -> Compiler String
buildYearMonthArchiveField ya ma pageYear = fmap TL.unpack $ renderTextT $
    ul_ [class_ "archive-tree"] $ do
        let yearMap = sortBy (flip compare `on` (read :: String -> Int) . fst) $ archivesMap ya
            getUrl = lift . fmap (toUrl . fromMaybe "#") . getRoute

        forM_ yearMap $ \(year, yids) ->
            li_ $ do
                let monthMap = sortBy (flip compare `on` (read :: String -> Int) . snd . fst) $
                        filter ((== year) . fst . fst) $ archivesMap ma
                    treeLael = T.pack $ "tree-label-" ++ year

                input_ $ [class_ "tree-toggle", type_ "checkbox", id_ treeLael] ++
                    [checked_ | Just year == pageYear]
                label_ [class_ "tree-toggle-button", for_ treeLael] $ do
                    i_ [classes_ ["fas", "fa-angle-right", "fa-fw"]] ""
                    i_ [classes_ ["fas", "fa-angle-down", "fa-fw"]] ""

                yurl <- getUrl $ archivesMakeId ya year
                a_ [href_ (T.pack yurl)] $
                    toHtml $ year ++ " (" ++ show (length yids) ++ ")"

                ul_ [class_ "tree-child"] $
                    forM_ monthMap $ \(mk@(_, month), mids) ->
                        li_ $ do
                            murl <- getUrl $ archivesMakeId ma mk
                            a_ [href_ (T.pack murl)] $
                                toHtml $ year ++ "/" ++ month ++  " (" ++ show (length mids) ++ ")"

yearMonthArchiveField :: String -> YearlyArchives -> MonthlyArchives -> Maybe String -> Context a
yearMonthArchiveField key ya ma s = field key
    $ const
    $ buildYearMonthArchiveField ya ma s

searchBoxResultField :: Context String
searchBoxResultField = constField "body" $
    TL.unpack $ renderText $ div_ [class_ "gcse-searchresults-only"] mempty

haskellJpLogo :: Html ()
haskellJpLogo =
    a_ [ href_ "https://haskell.jp/blog/posts/links.html#roki.dev/roki.log/" ] $
        img_ [
            width_ "234"
          , class_ "mt-1"
          , src_ "https://haskell.jp/img/supported-by-haskell-jp.svg"
          , alt_"Supported By Haskell-jp."
          ]

gAdSenseBody :: Html ()
gAdSenseBody = do
    script_ [
        async_ mempty
      , src_ "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
      ]
        TL.empty
    ins_ [
        class_ "adsbygoogle"
      , style_ "display:block"
      , data_ "ad-client" "ca-pub-5658861742931397"
      , data_ "ad-slot" "9559934596"
      , data_ "ad-format" "auto"
      , data_ "full-width-responsive" "true"
      ]
        mempty
    script_ "(adsbygoogle = window.adsbygoogle || []).push({});"

gAdSenseHeader :: Html ()
gAdSenseHeader = script_ [
    data_ "ad-client" "ca-pub-5658861742931397"
  , async_ mempty
  , src_ "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
  ]
    TL.empty

