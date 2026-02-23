{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings #-}
module Rules.DisneyExperienceSummary (rules) where

import           Control.Monad                    (filterM, forM_)
import           Control.Monad.Reader             (asks)
import           Control.Monad.Trans              (MonadTrans (..))
import           Data.Aeson                       (encode)
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit)
import           Data.Disney.Experience.Generator
import           Data.List                        (foldl', intercalate,
                                                   isPrefixOf, isSuffixOf, nub,
                                                   sort, sortBy, sortOn)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe)
import           Data.Ord                         (comparing)
import           Data.String                      (IsString (..))
import           Data.Time                        (defaultTimeLocale,
                                                   formatTime)
import           Dhall                            (FromDhall, Generic, Natural,
                                                   auto, input)
import           Hakyll
import           System.Directory                 (doesFileExist,
                                                   getModificationTime,
                                                   listDirectory)
import           System.FilePath                  (joinPath, (</>))
import qualified System.FilePath.Posix            as Posix
import           System.FilePath.Posix            (takeBaseName)

import           Config                           (contentsRoot, readerOptions)
import           Contexts                         (siteCtx)
import           Media.SVG                        (mermaidTransform)
import           Rules.PageType
import           Text.HTML.TagSoup                (innerText, parseTags)
import           Text.Pandoc.Walk                 (walkM)
import           Utils                            (mconcatM,
                                                   modifyExternalLinkAttr)
import qualified Vendor.FontAwesome               as FA

data Favorite = Favorite {
    text     :: String
  , category :: String
  , link     :: Maybe String
  } deriving (Generic, Show)

instance FromDhall Favorite

-- ホテルの詳細情報の階層構造（任意階層対応）
data HotelDetail
  = HDText String
  | HDNode { hdLabel :: String, hdChildren :: [HotelDetail] }
  deriving (Generic, Show)

-- ホテル情報のデータ構造
data Hotel = Hotel {
    hotelCode  :: String
  , stays      :: Natural
  , details    :: [HotelDetail]
  , hotelColor :: String
  } deriving (Show)

-- Dhallから読み込むための中間データ構造
data HotelRaw = HotelRaw {
    hotelCodeRaw  :: String
  , staysRaw      :: Natural
  , detailsRaw    :: [[String]]
  , hotelColorRaw :: String
  } deriving (Generic, Show)

instance FromDhall HotelRaw

buildHotelDetails :: [[String]] -> [HotelDetail]
buildHotelDetails = foldl' (flip insertPath) []

insertPath :: [String] -> [HotelDetail] -> [HotelDetail]
insertPath [] forest           = forest
insertPath [leaf] forest       = insertLeaf leaf forest
insertPath (label:rest) forest = insertBranch label rest forest

insertLeaf :: String -> [HotelDetail] -> [HotelDetail]
insertLeaf leaf [] = [HDText leaf]
insertLeaf leaf (HDText txt : xs)
    | txt == leaf = HDText txt : xs
    | otherwise = HDText txt : insertLeaf leaf xs
insertLeaf leaf (node@HDNode{} : xs) = node : insertLeaf leaf xs

insertBranch :: String -> [String] -> [HotelDetail] -> [HotelDetail]
insertBranch label rest [] =
    [HDNode { hdLabel = label, hdChildren = insertPath rest [] }]
insertBranch label rest (node@HDNode { hdLabel = lbl, hdChildren = chldn } : xs)
    | lbl == label = HDNode lbl (insertPath rest chldn) : xs
    | otherwise = node : insertBranch label rest xs
insertBranch label rest (leaf@HDText{} : xs) = leaf : insertBranch label rest xs

-- タグ設定のデータ構造
data TagConfig = TagConfig {
    mapKey   :: String
  , mapValue :: TagInfo
  } deriving (Generic, Show)

data TagInfo = TagInfo {
    color :: String
  , url   :: String
  } deriving (Generic, Show)

instance FromDhall TagConfig
instance FromDhall TagInfo

data LogImage = LogImage {
    imageUrl :: String
  , imageAlt :: String
  } deriving (Show)

data Offer = Offer {
    offerId          :: String
  , offerTitle       :: String
  , offerUrl         :: String
  , offerCtaLabel    :: String
  , offerDescription :: Maybe String
  , offerUtmCampaign :: Maybe String
  , offerIsActive    :: Bool
  } deriving (Generic, Show)

instance FromDhall Offer

-- Dhallファイルからタグ設定を読み込み
loadDisneyTags :: IO [TagConfig]
loadDisneyTags = input auto "./contents/config/disney/Tags.dhall"

-- タグ設定をMapに変換
tagConfigMap :: IO (M.Map String (String, String))
tagConfigMap = do
    tags <- loadDisneyTags
    return $ M.fromList $ map (\tag -> (mapKey tag, (color $ mapValue tag, url $ mapValue tag))) tags

getTag :: String -> M.Map String (String, String) -> (String, String)
getTag = M.findWithDefault ("#363636", "#")

-- タグの色を取得
getTagColor :: String -> M.Map String (String, String) -> String
getTagColor = (.) fst . getTag

-- タグのリンクを取得
getTagLink :: String -> M.Map String (String, String) -> String
getTagLink = (.) snd . getTag

-- メタデータ用のトリム関数
trimMeta :: String -> String
trimMeta = f . f
  where f = reverse . dropWhile (`elem` (" \n\r\t" :: String))

-- HTMLタグを除去してプレーンテキストを取得
stripHtmlTags :: String -> String
stripHtmlTags = innerText . parseTags

-- SNSリンクのメタデータを処理するためのフィールド
snsLinksField :: String -> Context String
snsLinksField snsType = listFieldWith (snsType ++ "-links") (field "url" (return . itemBody)) $ \item -> do
    mUrls <- getMetadataField (itemIdentifier item) snsType
    case mUrls of
        Just urlsStr -> return $ map (\url -> Item (fromString url) (trimMeta url)) (splitAll "," urlsStr)
        Nothing     -> return []

-- タグのメタデータを処理するためのフィールド
disneyTagsField :: M.Map String (String, String) -> Context String
disneyTagsField tagConfig = listFieldWith "disney-tags-list" tagCtx $ \item -> do
    mTags <- getMetadataField (itemIdentifier item) "disney-tags"
    case mTags of
        Just tagsStr -> return $ map (\tag -> Item (fromString tag) tag) $ sort $ filter (not . null) $ map trimMeta (splitAll "," tagsStr)
        Nothing      -> return []
  where
    tagCtx = field "name" (return . itemBody)
          <> field "color" (return . flip getTagColor tagConfig . itemBody)
          <> field "link" (return . flip getTagLink tagConfig . itemBody)

disneyExperienceSummaryRoot :: FilePath
disneyExperienceSummaryRoot = joinPath [contentsRoot, "disney_experience_summary"]

aboutIdent :: Identifier
aboutIdent = fromString
    $ joinPath [disneyExperienceSummaryRoot, "about.md"]

disneyLogsPattern :: Pattern
disneyLogsPattern = fromRegex $ mconcat
    [ "(^"
    , joinPath [disneyExperienceSummaryRoot, "logs", "[0-9]+.md"]
    , "$)|(^"
    , joinPath [disneyExperienceSummaryRoot, "logs", "[0-9]+", "index.md"]
    , "$)"
    ]

sortByNum :: [Item a] -> [Item a]
sortByNum = sortBy
    $ flip
    $ comparing
    $ fromMaybe (0 :: Int) . extractLogNumber . toFilePath . itemIdentifier
  where
    extractLogNumber :: FilePath -> Maybe Int
    extractLogNumber filePath =
        let candidates = [takeBaseName filePath, takeBaseName (Posix.takeDirectory filePath)]
            digitsOnly = filter (all isDigit) candidates
        in case digitsOnly of
            (x:_) -> Just (read x)
            _     -> Nothing

mdRule :: Snapshot
    -> Pattern
    -> PageConfReader Rules ()
mdRule ss pat = do
    wOpt <- asks pcWriterOpt
    katexRender <- asks pcKaTeXRender
    faIcons <- asks pcFaIcons
    lift $ match pat $ compile $ do
        pandocCompilerWithTransformM readerOptions wOpt (walkM mermaidTransform)
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons
            >>= katexRender
            >>= saveSnapshot ss

loadDisneyFavorites :: IO [Favorite]
loadDisneyFavorites = input auto "./contents/config/disney/Favorites.dhall"

loadDisneyOffers :: IO [Offer]
loadDisneyOffers = input auto "./contents/config/disney/Offers.dhall"

offerRedirectPath :: Offer -> FilePath
offerRedirectPath offer = joinPath ["disney_experience_summary", "go", offerId offer, "index.html"]

offerTrackingPath :: Offer -> String
offerTrackingPath offer = Posix.makeRelative "disney_experience_summary" (offerRedirectPath offer)

appendQueryParams :: String -> [(String, String)] -> String
appendQueryParams rawUrl queryParams
    | null queryParams = rawUrl
    | otherwise        = basePart ++ queryJoiner ++ serializedParams ++ fragmentPart
  where
    (basePart, fragmentPart) = breakAt '#'
    serializedParams = intercalate "&" $ map (\(k, v) -> k ++ "=" ++ v) queryParams
    hasQuery = '?' `elem` basePart
    queryJoiner
        | not hasQuery = "?"
        | null basePart = ""
        | last basePart `elem` ("?&" :: String) = ""
        | otherwise = "&"

    breakAt :: Char -> (String, String)
    breakAt delimiter = case break (== delimiter) rawUrl of
        (prefix, [])     -> (prefix, "")
        (prefix, suffix) -> (prefix, suffix)

offerDestinationUrl :: Offer -> String
offerDestinationUrl offer =
    appendQueryParams
        (offerUrl offer)
        [ ("utm_source", "roki_disney")
        , ("utm_medium", "referral")
        , ("utm_campaign", fromMaybe ("offer_" ++ offerId offer) (offerUtmCampaign offer))
        ]

escapeHtmlAttr :: String -> String
escapeHtmlAttr = concatMap escapeChar
  where
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar ch   = [ch]

escapeHtmlText :: String -> String
escapeHtmlText = escapeHtmlAttr

offerRedirectHtml :: String -> String
offerRedirectHtml destinationUrl =
    "<!doctype html><html lang=\"ja\"><head><meta charset=\"utf-8\">"
    ++ "<meta name=\"robots\" content=\"noindex,nofollow\">"
    ++ "<meta http-equiv=\"refresh\" content=\"0;url="
    ++ escapeHtmlAttr destinationUrl
    ++ "\">"
    ++ "<title>Redirecting...</title>"
    ++ "<script>window.location.replace("
    ++ show destinationUrl
    ++ ");</script>"
    ++ "</head><body><p>遷移中です。"
    ++ "<a href=\""
    ++ escapeHtmlAttr destinationUrl
    ++ "\">こちら</a></p></body></html>"

renderFeaturedOfferHtml :: [Offer] -> String
renderFeaturedOfferHtml offers = case offers of
    []        -> ""
    (offer:_) ->
        let descriptionHtml = case offerDescription offer of
                Just desc | not (null desc) ->
                    "<span class=\"featured-offer-description\">"
                    ++ escapeHtmlText desc
                    ++ "</span>"
                _ ->
                    ""
        in "<a class=\"featured-offer-link offer-link\""
            ++ " href=\"" ++ escapeHtmlAttr (offerTrackingPath offer) ++ "\""
            ++ " target=\"_blank\""
            ++ " rel=\"sponsored nofollow noopener\""
            ++ " data-offer-id=\"" ++ escapeHtmlAttr (offerId offer) ++ "\""
            ++ " data-offer-title=\"" ++ escapeHtmlAttr (offerTitle offer) ++ "\""
            ++ " data-offer-placement=\"list-featured\">"
            ++ "<span class=\"featured-offer-badge\">PR</span>"
            ++ "<span class=\"featured-offer-main\">"
            ++ "<span class=\"featured-offer-title\">"
            ++ escapeHtmlText (offerTitle offer)
            ++ "</span>"
            ++ descriptionHtml
            ++ "</span>"
            ++ "<span class=\"featured-offer-cta\">"
            ++ escapeHtmlText (offerCtaLabel offer)
            ++ "</span>"
            ++ "</a>"

-- Dhallファイルからホテル情報を読み込み
loadDisneyHotels :: IO [Hotel]
loadDisneyHotels =
    map convert <$> (input auto "./contents/config/disney/Hotels.dhall" :: IO [HotelRaw])
  where
    convert (HotelRaw codeRaw staysRaw detailPathsRaw colorRaw) =
        Hotel
            { hotelCode = codeRaw
            , stays = staysRaw
            , details = buildHotelDetails detailPathsRaw
            , hotelColor = colorRaw
            }

-- Hotels.dhallの最終更新日を取得
getHotelsLastModified :: IO String
getHotelsLastModified = do
    modTime <- getModificationTime "./contents/config/disney/Hotels.dhall"
    return $ formatTime defaultTimeLocale "%Y/%m/%d" modTime

-- ディズニーログの最終更新日を取得（最も新しいファイルの日付）
getLogsLastModified :: IO String
getLogsLastModified = do
    let logsDir = joinPath [contentsRoot, "disney_experience_summary", "logs"]
    entries <- listDirectory logsDir
    let topLevelMdFiles = [logsDir </> fileName | fileName <- entries, ".md" `isSuffixOf` fileName]
        indexMdCandidates = [logsDir </> dirName </> "index.md" | dirName <- entries, all isDigit dirName]
    existingIndexMdFiles <- filterM doesFileExist indexMdCandidates
    let mdFiles = topLevelMdFiles ++ existingIndexMdFiles
    if null mdFiles
        then return "-"
        else do
            modTimes <- mapM getModificationTime mdFiles
            let latestTime = maximum modTimes
            return $ formatTime defaultTimeLocale "%Y/%m/%d" latestTime

-- ログエントリ用のコンテキストを作成
disneyLogCtx :: M.Map String (String, String) -> Context String
disneyLogCtx tagConfig = mconcat
    [ metadataField
    , bodyField "log-body"
    , field "log-body-text" $ return . stripHtmlTags . itemBody
    , snsLinksField "youtube"
    , snsLinksField "instagram"
    , snsLinksField "x"
    , snsLinksField "note"
    , imageItemsField
    , disneyTagsField tagConfig
    , aiGeneratedField
    ]
  where
    imageItemsField = listFieldWith "image-items" imageCtx $ \item -> do
        imagePaths <- extractImagePaths item
        return $ zipWith toImageItem [1 :: Int ..] $ map (resolveImageUrl item) imagePaths

    imageCtx = mconcat
        [ field "url" (return . imageUrl . itemBody)
        , field "alt" (return . imageAlt . itemBody)
        ]

    toImageItem :: Int -> String -> Item LogImage
    toImageItem idx url =
        Item
            (fromString $ "image-item-" ++ show idx)
            LogImage {
                imageUrl = url
              , imageAlt = "体験録画像 " ++ show idx
              }

    extractImagePaths :: Item String -> Compiler [String]
    extractImagePaths item = do
        mImagePaths <- getMetadataField (itemIdentifier item) "images"
        case mImagePaths of
            Just imagePathsStr -> return $ filter (not . null) $ map trimMeta $ splitAll "," imagePathsStr
            Nothing            -> return []

    resolveImageUrl :: Item String -> String -> String
    resolveImageUrl item path
        | null path = path
        | "http://" `isPrefixOf` path = path
        | "https://" `isPrefixOf` path = path
        | Posix.isAbsolute path = path
        | otherwise = toPublicUrl
            $ Posix.normalise
            $ Posix.takeDirectory (toFilePath $ itemIdentifier item) </> path

    toPublicUrl :: FilePath -> String
    toPublicUrl filePath
        | null filePath = filePath
        | Posix.isAbsolute filePath = filePath
        | (contentsRoot ++ "/") `isPrefixOf` filePath =
            let publicPath = dropWhile (== '/') $ drop (length contentsRoot) filePath
            in Posix.makeRelative "disney_experience_summary" publicPath
        | otherwise = filePath

    aiGeneratedField = listFieldWith "ai-generated-badges" aiGeneratedBadgeCtx $ \item -> do
        mAiGeneratedBy <- getMetadataField (itemIdentifier item) "ai-generated-by"
        case fmap trimMeta mAiGeneratedBy of
            Just modelName
                | not (null modelName) -> return [Item (fromString "ai") modelName]
            _ -> return []

    aiGeneratedBadgeCtx :: Context String
    aiGeneratedBadgeCtx = mconcat
        [ field "badge-text" $ const $ return "Generated by AI"
        , field "model-name" (return . itemBody)
        ]

-- ホテル情報用のコンテキストを作成
hotelCtx :: Context Hotel
hotelCtx = mconcat
    [ field "hotel-code" (return . hotelCode . itemBody)
    , field "stays-count" (return . show . stays . itemBody)
    , field "hotel-color" (return . hotelColor . itemBody)
    , field "hotel-details-html" $ \item ->
        return $ renderHotelDetails (details $ itemBody item)
    ]
  where
    -- 階層構造をHTMLに変換
    renderHotelDetails :: [HotelDetail] -> String
    renderHotelDetails = concatMap (renderDetail 0)

    renderDetail :: Int -> HotelDetail -> String
    renderDetail level (HDText text) = renderSpan level text
    renderDetail level (HDNode {hdLabel = lbl, hdChildren = chldn}) =
        renderSpan level lbl ++ concatMap (renderDetail (level + 1)) chldn

    renderSpan :: Int -> String -> String
    renderSpan level text =
        let classLevel = min level 3
        in "<span class=\"hotel-detail-item hotel-detail-level-" ++ show classLevel ++ "\">" ++ text ++ "</span>"

rules :: PageConfReader Rules ()
rules = do
    let items = disneyLogsPattern : map (fromList . (:[]))
            [ aboutIdent
            ]
    mapM_ (mdRule disneyExperienceSummarySnapshot) items
    faIcons <- asks pcFaIcons
    isPreview <- asks pcIsPreview
    favorites <- lift $ preprocess loadDisneyFavorites
    hotels <- lift $ preprocess loadDisneyHotels
    offers <- lift $ preprocess loadDisneyOffers
    hotelsLastModified <- lift $ preprocess getHotelsLastModified
    logsLastModified <- lift $ preprocess getLogsLastModified
    tagConfig <- lift $ preprocess tagConfigMap
    let activeOffers = filter offerIsActive offers
        featuredOfferHtml = renderFeaturedOfferHtml activeOffers
    let totalStays = sum $ map stays hotels
    lift $ do
        -- フォントファイルのコピー
        match (fromGlob $ joinPath [contentsRoot, "fonts", "*.otf"]) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler

        -- Disney体験録に紐づく画像などのアセットをコピー
        match ((fromGlob $ joinPath [disneyExperienceSummaryRoot, "logs", "**"]) .&&. complement disneyLogsPattern) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler

        forM_ activeOffers $ \offer -> do
            create [fromFilePath $ offerRedirectPath offer] $ do
                route idRoute
                compile $ makeItem $ offerRedirectHtml $ offerDestinationUrl offer

        -- JSON可視化データの生成
        create [fromFilePath "data/disney-experience-visualization.json"] $ do
            route idRoute
            compile $ do
                disneyLogs <- loadAllSnapshots disneyLogsPattern disneyExperienceSummarySnapshot :: Compiler [Item String]
                vizData <- generateVisualizationData disneyLogs
                makeItem $ BL.toStrict $ encode vizData

        match disneyExperienceSummaryJPPath $ do
            route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
            compile $ do
                disneyLogs <- sortByNum <$> loadAllSnapshots disneyLogsPattern disneyExperienceSummarySnapshot
                let totalLogs = length disneyLogs
                -- ユニークなタグリストを作成
                uniqueTags <- do
                    allTags <- sequence $ map (\logItem -> do
                        mTags <- getMetadataField (itemIdentifier logItem) "disney-tags"
                        case mTags of
                            Just tagsStr -> return $ map trimMeta $ filter (not . null) $ splitAll "," tagsStr
                            Nothing      -> return []
                        ) disneyLogs
                    return $ sort $ nub $ concat allTags

                disneyExperienceSummaryCtx <- mconcatM [
                    pure $ constField "title" "Ponchi's Disney Journey"
                  , pure $ constField "font_path" "../fonts/waltograph42.otf"
                  , pure $ constField "is_preview" (show isPreview)
                  , pure $ listField "additional-css" (field "css" (return . itemBody)) (return $ map (\css -> Item (fromString css) css) ["../style/disney_experience_summary_only.css"])
                 , pure $ listField "additional-js" (field "js" (return . itemBody)) (return $ map (\js -> Item (fromString js) js) ["https://d3js.org/d3.v7.min.js", "../js/disney-tag-filter.js", "../js/disney-experience-visualizations.js"])
                  , pure siteCtx
                  , pure defaultContext
                  , constField "about-body"
                        <$> loadSnapshotBody aboutIdent disneyExperienceSummarySnapshot
                  , pure $ listField "disney-logs" (disneyLogCtx tagConfig) (return disneyLogs)
                  , pure $ constField "featured-offer-html" featuredOfferHtml
                  , pure $ listField "unique-tags" (field "name" (return . itemBody) <> field "color" (return . flip getTagColor tagConfig . itemBody) <> field "link" (return . flip getTagLink tagConfig . itemBody)) (return $ map (\tag -> Item (fromString tag) tag) uniqueTags)
                  , pure $ favoritesListField "favorite-works" "works" favorites
                  , pure $ favoritesListField "favorite-characters" "characters" favorites
                  , pure $ favoritesListField "favorite-park-contents" "park-contents" favorites
                  , pure $ listField "hotel-stays" hotelCtx (return $ map (\h -> Item (fromString $ hotelCode h) h) hotels)
                  , pure $ constField "hotels-last-modified" hotelsLastModified
                  , pure $ constField "hotels-total-stays" (show totalStays)
                  , pure $ constField "logs-total-count" (show totalLogs)
                  , pure $ constField "logs-last-modified" logsLastModified
                      ]
                getResourceBody
                    >>= applyAsTemplate disneyExperienceSummaryCtx
                    >>= loadAndApplyTemplate rootTemplate disneyExperienceSummaryCtx
                    >>= relativizeUrls
                    >>= FA.render faIcons

        createRedirects [
            (fromFilePath $ joinPath ["disney_experience_summary", "index.html"], joinPath ["/", "disney_experience_summary", "jp.html"]),
            (fromFilePath $ joinPath ["disney", "index.html"], joinPath ["/", "disney_experience_summary", "jp.html"])
          ]
    where
        disneyExperienceSummarySnapshot = "disneyExperienceSummarySS"
        disneyExperienceSummaryJPPath = fromGlob $ joinPath [contentsRoot, "pages", "disney_experience_summary", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]

        favoritesListField :: String -> String -> [Favorite] -> Context String
        favoritesListField fieldName categoryName favs =
            listField fieldName favoritesCtx $ return $ favoriteItems categoryName favs

        favoritesCtx :: Context Favorite
        favoritesCtx = field "text" (return . text . itemBody)
            <> field "link" (return . maybe "" id . link . itemBody)

        favoriteItems :: String -> [Favorite] -> [Item Favorite]
        favoriteItems categoryName favs =
            map (\f -> Item (fromString $ text f) f)
                $ sortOn text
                $ filter ((== categoryName) . category) favs
