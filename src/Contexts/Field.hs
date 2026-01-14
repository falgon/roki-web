{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field (
    localDateField
  , iso8601DateField
  , jsonLdArticleField
  , breadcrumbField
  , tagsField'
  , tagCloudField'
  , descriptionField
  , imageField
  , ogImageField
  , yearMonthArchiveField
  , searchBoxResultField
) where

import           Control.Monad           (forM_, liftM2)
import           Control.Monad.Trans     (lift)
import           Data.Aeson              (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy    as BL
import           Data.Function           (on)
import           Data.Functor            ((<&>))
import           Data.List               (inits, intercalate, isPrefixOf,
                                          isSuffixOf, sortBy)
import           Data.List.Extra         (mconcatMap)
import           Data.Maybe              (catMaybes, fromMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Format        (TimeLocale (..), formatTime)
import           Data.Time.LocalTime     (TimeZone (..), utcToLocalTime)
import           Hakyll                  hiding (isExternal)
import           Lucid.Base              (Html, ToHtml (..), renderText,
                                          renderTextT, toHtml)
import           Lucid.Html5
import           System.FilePath         (splitDirectories)
import qualified Text.HTML.TagSoup       as TS

import           Archives                (Archives (..), MonthlyArchives,
                                          YearlyArchives)
import           Config.Site             (baseUrl, defaultTimeLocale', siteName,
                                          timeZoneJST)

toLink :: String -> String -> Html ()
toLink text path = a_ [href_ (T.pack $ toUrl path)] $ span_ $ toHtml text

localDateField :: TimeLocale -> TimeZone -> String -> String -> Context a
localDateField locale zone key format = field key $
    fmap (formatTime locale format . utcToLocalTime zone) . getItemUTC locale . itemIdentifier

iso8601DateField :: String -> Context String
iso8601DateField key = localDateField defaultTimeLocale' timeZoneJST key "%Y-%m-%dT%H:%M:%S%Ez"

-- | JSON-LD Article Schema (BlogPosting) を生成するフィールド
-- schema.org仕様に準拠したJSON-LDを出力
jsonLdArticleField :: String -> Context String
jsonLdArticleField key = field key $ \item -> do
    -- メタデータからタイトルと更新日を取得
    metadata <- getMetadata (itemIdentifier item)
    let mTitle = lookupString "title" metadata
        mUpdated = lookupString "updated" metadata

    -- 日付をISO 8601形式で取得
    let iso8601Format = "%Y-%m-%dT%H:%M:%S%Ez"
        formatDate = formatTime defaultTimeLocale' iso8601Format . utcToLocalTime timeZoneJST
    publishedDate <- formatDate <$> getItemUTC defaultTimeLocale' (itemIdentifier item)

    -- dateModifiedはupdatedメタデータがあればそれを使用、なければpublishedDateと同じ
    let dateModified = fromMaybe publishedDate mUpdated

    -- URLを取得（絶対URL）
    mRoute <- getRoute (itemIdentifier item)
    let articleUrl = maybe "" ((baseUrl <>) . toUrl) mRoute

    -- リソースボディを1回だけ取得してパフォーマンス改善
    resourceBody <- itemBody <$> getResourceBody

    -- 説明文を取得（最初の150文字）
    let description = take 150 $ concat $ lines resourceBody

    -- 画像URLを取得（なければデフォルト画像）
    let defaultImage = baseUrl <> "/images/avator/prof1000x1000.png"
        images = extractImagesFromHtml $ TS.parseTags resourceBody
        imageUrl = case images of
            []      -> defaultImage
            (src:_) -> if isExternal src then src else baseUrl <> src

    case mTitle of
        Nothing -> noResult $ "Field " ++ key ++ ": title not found"
        Just title -> return $ TL.unpack $ TLE.decodeUtf8 $ encode $ object
            [ "@context" .= ("https://schema.org" :: String)
            , "@type" .= ("BlogPosting" :: String)
            , "headline" .= title
            , "image" .= imageUrl
            , "datePublished" .= publishedDate
            , "dateModified" .= dateModified
            , "author" .= object
                [ "@type" .= ("Person" :: String)
                , "name" .= ("Roki" :: String)
                , "url" .= baseUrl
                ]
            , "publisher" .= object
                [ "@type" .= ("Organization" :: String)
                , "name" .= siteName
                , "logo" .= object
                    [ "@type" .= ("ImageObject" :: String)
                    , "url" .= defaultImage
                    ]
                ]
            , "description" .= description
            , "url" .= articleUrl
            ]

-- | BreadcrumbList の個別要素を生成
mkListItem :: Int -> String -> String -> Value
mkListItem pos name url = object
    [ "@type" .= ("ListItem" :: String)
    , "position" .= pos
    , "name" .= name
    , "item" .= url
    ]

-- | パスセグメントからパンくず要素のリストを構築
-- ホーム要素を先頭に追加し、各セグメントに累積的な URL を設定
buildBreadcrumbs :: String -> String -> [String] -> [Value]
buildBreadcrumbs base finalTitle segments =
    homeItem : zipWith3 mkListItem [2 ..] names urls
  where
    homeItem = mkListItem 1 "ホーム" base
    names = init segments ++ [finalTitle]
    urls = map ((base <>) . ("/" <>) . intercalate "/") $ tail $ inits segments

-- | JSON-LD BreadcrumbList スキーマを生成するフィールド
-- パス構造から階層的なパンくずリストを構築し、schema.org 仕様に準拠した JSON-LD を出力
breadcrumbField :: String -> Context String
breadcrumbField key = field key $ \item ->
    getRoute (itemIdentifier item) >>= \mRoute ->
        case mRoute of
            Nothing -> noResult $ "Field " ++ key ++ ": route not found"
            Just route ->
                let segments = filter (not . null) $ splitDirectories route
                in case segments of
                    [] -> noResult $ "Field " ++ key ++ ": empty route"
                    _ -> do
                        metadata <- getMetadata (itemIdentifier item)
                        let title = fromMaybe (last segments) $ lookupString "title" metadata
                            breadcrumbs = buildBreadcrumbs baseUrl title segments
                        return $ TL.unpack $ TLE.decodeUtf8 $ encode $ object
                            [ "@context" .= ("https://schema.org" :: String)
                            , "@type" .= ("BreadcrumbList" :: String)
                            , "itemListElement" .= breadcrumbs
                            ]

isExternal :: String -> Bool
isExternal url = "http://" `isPrefixOf` url || "https://" `isPrefixOf` url

-- | HTML文字列から画像URLを抽出する共通関数
-- 外部URL、SVG画像、空のsrc属性を除外
extractImagesFromHtml :: [TS.Tag String] -> [String]
extractImagesFromHtml = map (TS.fromAttrib "src") . filter isValidImage
  where
    isValidImage tag =
        let src = TS.fromAttrib "src" tag
        in TS.isTagOpenName "img" tag
            && not (null src)
            && not (isExternal src)
            && not (".svg" `isSuffixOf` src)

imageField :: String -> Context String
imageField key = field key $ \item ->
    case extractImagesFromHtml $ TS.parseTags $ itemBody item of
        [] -> noResult ("Field " ++ key ++ ": " ++ show (itemIdentifier item) ++ " has no image")
        (src:_) -> return src

ogImageField :: String -> String -> Context String
ogImageField key defaultImage = field key $ \item ->
    fmap (makeAbsoluteUrl . fromMaybe defaultImage)
        $ getMetadataField (itemIdentifier item) "og-image"
  where
    makeAbsoluteUrl :: String -> String
    makeAbsoluteUrl path
        | any (`isPrefixOf` path) ["http://", "https://"] = path
        | otherwise = "https://" ++ siteName ++ ensureLeadingSlash path

    ensureLeadingSlash :: String -> String
    ensureLeadingSlash s@('/':_) = s
    ensureLeadingSlash s         = '/' : s

descriptionField :: String -> Int -> Context String
descriptionField key len = field key $ const $
    take len . concat . lines . itemBody <$> getResourceBody

tagsField' :: String -> Tags -> Context a
tagsField' key tags = field key $ \item -> do
    links <- getTags (itemIdentifier item)
        >>= mapM (liftM2 (<$>) toLink' (getRoute . tagsMakeId tags))
        <&> catMaybes
    if null links
        then noResult ("Field " ++ key ++ ": tag not set (" ++ show (itemIdentifier item) ++ ")")
        else return $ TL.unpack $ renderText $ mconcatMap (span_ [class_ "tag is-dark"]) links
    where
        toLink' tag = fmap (toLink tag)

tagCloudField' :: String -> Tags -> Context a
tagCloudField' key tags = field key $ const $
    TL.unpack . renderText . div_ [class_ "tags"] . toHtmlRaw <$> renderTags toLink' concat tags
    where
        toLink' tag path = const $ const $ const $
            TL.unpack $ renderText $ span_ [class_ "tag is-dark"] $ toLink tag path


{-# INLINE buildYearMonthArchiveField #-}
buildYearMonthArchiveField :: YearlyArchives
    -> MonthlyArchives
    -> Maybe String
    -> Compiler String
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

yearMonthArchiveField :: String
    -> YearlyArchives
    -> MonthlyArchives
    -> Maybe String
    -> Context a
yearMonthArchiveField key ya ma s = field key
    $ const
    $ buildYearMonthArchiveField ya ma s

searchBoxResultField :: Context String
searchBoxResultField = constField "body" $
    TL.unpack $ renderText $ div_ [class_ "gcse-searchresults-only"] mempty
