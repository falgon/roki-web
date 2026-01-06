{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings #-}
module Rules.DisneyExperienceSummary (rules) where

import           Control.Monad.Reader             (asks)
import           Control.Monad.Trans              (MonadTrans (..))
import           Data.Aeson                       (encode)
import qualified Data.ByteString.Lazy             as BL
import           Data.Disney.Experience.Generator
import           Data.List                        (foldl', nub, sort, sortBy,
                                                   sortOn)
import qualified Data.Map                         as M
import           Data.Ord                         (comparing)
import           Data.String                      (IsString (..))
import           Data.Time                        (defaultTimeLocale,
                                                   formatTime)
import           Dhall                            (FromDhall, Generic, Natural,
                                                   auto, input)
import           Hakyll
import           System.Directory                 (getModificationTime,
                                                   listDirectory)
import           System.FilePath                  (joinPath, (</>))
import           System.FilePath.Posix            (takeBaseName)

import           Config                           (contentsRoot, readerOptions)
import           Contexts                         (siteCtx)
import           Media.SVG                        (mermaidTransform)
import           Rules.PageType
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
    , "$)"
    ]

sortByNum :: [Item a] -> [Item a]
sortByNum = sortBy
    $ flip
    $ comparing
    $ (read :: String -> Int) . takeBaseName . toFilePath . itemIdentifier

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
    files <- listDirectory logsDir
    let mdFiles = filter (\f -> ".md" `isSuffixOf` f) files
    modTimes <- mapM (\f -> getModificationTime (logsDir </> f)) mdFiles
    let latestTime = maximum modTimes
    return $ formatTime defaultTimeLocale "%Y/%m/%d" latestTime
  where
    isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- ログエントリ用のコンテキストを作成
disneyLogCtx :: M.Map String (String, String) -> Context String
disneyLogCtx tagConfig = mconcat
    [ metadataField
    , bodyField "log-body"
    , snsLinksField "youtube"
    , snsLinksField "instagram"
    , snsLinksField "x"
    , snsLinksField "note"
    , disneyTagsField tagConfig
    , aiGeneratedField
    ]
  where
    aiGeneratedField = listFieldWith "ai-generated-badges" (field "badge-text" (return . itemBody)) $ \item -> do
        mAiGenerated <- getMetadataField (itemIdentifier item) "ai-generated"
        case mAiGenerated of
            Just "true" -> return [Item (fromString "ai") "Generated by AI"]
            _           -> return []

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
    hotelsLastModified <- lift $ preprocess getHotelsLastModified
    logsLastModified <- lift $ preprocess getLogsLastModified
    tagConfig <- lift $ preprocess tagConfigMap
    let totalStays = sum $ map stays hotels
    lift $ do
        -- フォントファイルのコピー
        match (fromGlob $ joinPath [contentsRoot, "fonts", "*.otf"]) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler

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
