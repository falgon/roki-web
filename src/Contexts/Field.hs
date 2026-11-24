{-# LANGUAGE OverloadedStrings #-}
module Contexts.Field (
    localDateField
  , iso8601DateField
  , jsonLdArticleField
  , jsonLdPersonField
  , jsonLdWebSiteField
  , breadcrumbField
  , tagsField'
  , tagCloudField'
  , postDescription
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
import           Data.List               (inits, isPrefixOf, isSuffixOf, sortBy)
import           Data.List.Extra         (mconcatMap)
import           Data.Maybe              (catMaybes, fromMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Format        (TimeLocale (..), formatTime)
import           Data.Time.LocalTime     (TimeZone (..), utcToLocalTime,
                                          utcToZonedTime)
import           Hakyll                  hiding (isExternal)
import           Lucid.Base              (Html, ToHtml (..), renderText,
                                          renderTextT, toHtml)
import           Lucid.Html5
import           System.FilePath         (joinPath, splitDirectories,
                                          takeDirectory, takeFileName, (</>))
import qualified Text.HTML.TagSoup       as TS
import qualified Text.HTML.TagSoup.Tree  as TT

import           Archives                (Archives (..), MonthlyArchives,
                                          YearlyArchives)
import           Config.Author           (authorDescriptionEn,
                                          authorDescriptionJa, authorGithub,
                                          authorJobTitle, authorName,
                                          authorStackOverflow, authorTwitter)
import           Config.Site             (baseUrl, defaultTimeLocale', siteName,
                                          timeZoneJST)

toLink :: String -> String -> Html ()
toLink text path = a_ [href_ (T.pack $ toUrl path)] $ span_ $ toHtml text

localDateField :: TimeLocale -> TimeZone -> String -> String -> Context a
localDateField locale zone key format = field key $
    fmap (formatTime locale format . utcToLocalTime zone) . getItemUTC locale . itemIdentifier

iso8601DateField :: String -> Context String
iso8601DateField key = field key $ publishedTime . itemIdentifier

publishedTime :: Identifier -> Compiler String
publishedTime = fmap formatDate . getItemUTC defaultTimeLocale'
  where
    formatDate = formatTime defaultTimeLocale' "%Y-%m-%dT%H:%M:%S%Ez" . utcToZonedTime timeZoneJST

-- | JSON-LD Article Schema (BlogPosting) を生成するフィールド
-- schema.org仕様に準拠したJSON-LDを出力
jsonLdArticleField :: String -> String -> Context String
jsonLdArticleField key description = field key $ \item -> do
    -- メタデータからタイトルと更新日を取得
    metadata <- getMetadata (itemIdentifier item)
    let mTitle = lookupString "title" metadata
        mUpdated = lookupString "updated" metadata

    -- 日付をISO 8601形式で取得
    publishedDate <- publishedTime (itemIdentifier item)

    -- dateModifiedはupdatedメタデータがあればそれを使用、なければpublishedDateと同じ
    let dateModified = fromMaybe publishedDate mUpdated

    -- URLを取得（絶対URL）
    mRoute <- getRoute (itemIdentifier item)
    let articleUrl = maybe "" ((baseUrl <>) . toUrl) mRoute

    resourceBody <- itemBody <$> getResourceBody

    -- 画像URLを取得（なければデフォルト画像）
    let defaultImage = baseUrl <> "/images/avator/prof1000x1000.png"
        images = extractImagesFromHtml $ TS.parseTags resourceBody
        imageUrl = case images of
            []      -> defaultImage
            (src:_) -> if isExternal src then src else baseUrl <> src

    case mTitle of
        Nothing -> noResult $ "Field " ++ key ++ ": title not found"
        Just title -> return $ renderJsonLd $ object
            [ "@context" .= ("https://schema.org" :: String)
            , "@type" .= ("BlogPosting" :: String)
            , "headline" .= title
            , "image" .= imageUrl
            , "datePublished" .= publishedDate
            , "dateModified" .= dateModified
            , "author" .= object
                [ "@type" .= ("Person" :: String)
                , "name" .= authorName
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

renderJsonLd :: Value -> String
renderJsonLd = TL.unpack . TL.replace "<" "\\u003c" . TLE.decodeUtf8 . encode

-- | JSON-LD Person Schema を生成するフィールド
-- サイト著者の構造化データを出力
-- langメタデータに応じてdescriptionを多言語切り替え
jsonLdPersonField :: String -> Context String
jsonLdPersonField key = field key $ \item -> do
    -- langメタデータを取得（デフォルトは"ja"）
    mLang <- getMetadataField (itemIdentifier item) "lang"
    let description = case mLang of
            Just "ja" -> authorDescriptionJa
            Just "en" -> authorDescriptionEn
            _         -> authorDescriptionJa  -- デフォルトは日本語
    return $ TL.unpack $ TLE.decodeUtf8 $ encode $ object
        [ "@context" .= ("https://schema.org" :: String)
        , "@type" .= ("Person" :: String)
        , "name" .= authorName
        , "url" .= baseUrl
        , "sameAs" .=
            [ "https://twitter.com/" <> authorTwitter
            , "https://github.com/" <> authorGithub
            , "https://stackoverflow.com/users/" <> authorStackOverflow
            ]
        , "jobTitle" .= authorJobTitle
        , "description" .= description
        ]

-- | JSON-LD WebSite Schema を生成するフィールド
-- サイト全体の構造化データと検索機能を出力
jsonLdWebSiteField :: String -> Context String
jsonLdWebSiteField key = constField key $ TL.unpack $ TLE.decodeUtf8 $ encode $ object
    [ "@context" .= ("https://schema.org" :: String)
    , "@type" .= ("WebSite" :: String)
    , "name" .= siteName
    , "url" .= baseUrl
    , "description" .= ("Roki's technical blog and hobby content" :: String)
    , "potentialAction" .= object
        [ "@type" .= ("SearchAction" :: String)
        , "target" .= (baseUrl <> "/roki.log/search.html?q={search_term_string}")
        , "query-input" .= ("required name=search_term_string" :: String)
        ]
    ]

-- | BreadcrumbList の個別要素を生成
mkListItem :: Int -> String -> String -> Value
mkListItem pos name url = object
    [ "@type" .= ("ListItem" :: String)
    , "position" .= pos
    , "name" .= name
    , "item" .= url
    ]

-- | JSON-LD BreadcrumbList スキーマを生成するフィールド
breadcrumbField :: String -> Context String
breadcrumbField key = field key $ \item -> do
    let ident = itemIdentifier item
    mRoute <- getRoute ident
    case mRoute of
        Nothing -> noResult $ "Field " ++ key ++ ": route not found"
        Just route -> do
            let parents = drop 1 $ inits $ splitDirectories $ takeDirectory route
                pageUrl = absoluteUrl route
            archives <- filter ((/= pageUrl) . snd) . catMaybes <$> mapM archiveCrumb parents
            title <- fromMaybe (takeFileName route) <$> getMetadataField ident "title"
            let crumbs = ("ホーム", baseUrl) : archives ++ [(title, pageUrl)]
            return $ renderJsonLd $ object
                [ "@context" .= ("https://schema.org" :: String)
                , "@type" .= ("BreadcrumbList" :: String)
                , "itemListElement" .= zipWith (uncurry . mkListItem) [1 ..] crumbs
                ]
  where
    absoluteUrl = (baseUrl <>) . toUrl
    archiveCrumb parts = fmap ((,) (last parts) . absoluteUrl)
        <$> getRoute (fromFilePath $ joinPath parts </> "index.html")

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

postDescription :: Item String -> Compiler String
postDescription item = do
    explicit <- getMetadataField (itemIdentifier item) "description"
    let provided = maybe [] words explicit
        description = if null provided then words (bodyText $ itemBody item) else provided
    pure $ take 150 $ unwords description
  where
    bodyText = concatMap nodeText . TT.tagTree . TS.canonicalizeTags . TS.parseTags
    nodeText (TT.TagLeaf (TS.TagText value)) = value
    nodeText (TT.TagLeaf (TS.TagOpen name _))
        | name `elem` ["br", "hr"] = " "
    nodeText (TT.TagBranch name attrs children)
        | name `elem` ["script", "style"] = " "
        | name == "div" && lookup "id" attrs == Just "toc" = " "
        | name `elem` textBlocks = " " <> content <> " "
        | otherwise = content
      where
        content = concatMap nodeText children
    nodeText _ = ""
    textBlocks = ["blockquote", "div", "h1", "h2", "h3", "h4", "h5", "h6",
                  "li", "p", "pre", "td", "th"]

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
