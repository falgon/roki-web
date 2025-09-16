{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Rules.DisneyExperienceSummary (rules) where

import           Control.Monad.Reader  (asks)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.List             (nub, sort, sortBy)
import qualified Data.Map              as M
import           Data.Ord              (comparing)
import           Data.String           (IsString (..))
import           Dhall                 (FromDhall, Generic, auto, input)
import           Hakyll
import           System.FilePath       (joinPath, (</>))
import           System.FilePath.Posix (takeBaseName)

import           Config                (contentsRoot, readerOptions)
import           Contexts              (siteCtx)
import           Media.SVG             (mermaidTransform)
import           Rules.PageType
import           Text.Pandoc.Walk      (walkM)
import           Utils                 (mconcatM, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome    as FA

data Favorite = Favorite {
    text     :: String
  , category :: String
  , link     :: Maybe String
  } deriving (Generic, Show)

instance FromDhall Favorite

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

rules :: PageConfReader Rules ()
rules = do
    let items = disneyLogsPattern : map (fromList . (:[]))
            [ aboutIdent
            ]
    mapM_ (mdRule disneyExperienceSummarySnapshot) items
    faIcons <- asks pcFaIcons
    isPreview <- asks pcIsPreview
    favorites <- lift $ preprocess loadDisneyFavorites
    tagConfig <- lift $ preprocess tagConfigMap
    lift $ do
        -- フォントファイルのコピー
        match (fromGlob $ joinPath [contentsRoot, "fonts", "*.otf"]) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler

        match disneyExperienceSummaryJPPath $ do
            route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
            compile $ do
                disneyLogs <- sortByNum <$> loadAllSnapshots disneyLogsPattern disneyExperienceSummarySnapshot
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
                  , pure $ listField "additional-js" (field "js" (return . itemBody)) (return $ map (\js -> Item (fromString js) js) ["../js/disney-tag-filter.js"])
                  , pure siteCtx
                  , pure defaultContext
                  , constField "about-body"
                        <$> loadSnapshotBody aboutIdent disneyExperienceSummarySnapshot
                  , pure $ listField "disney-logs" (disneyLogCtx tagConfig) (return disneyLogs)
                  , pure $ listField "unique-tags" (field "name" (return . itemBody) <> field "color" (return . flip getTagColor tagConfig . itemBody) <> field "link" (return . flip getTagLink tagConfig . itemBody)) (return $ map (\tag -> Item (fromString tag) tag) uniqueTags)
                  , pure $ listField "favorite-works" (field "text" (return . text . itemBody) <> field "link" (return . maybe "" id . link . itemBody)) (return $ map (\f -> Item (fromString $ text f) f) $ filter ((== "works") . category) favorites)
                  , pure $ listField "favorite-characters" (field "text" (return . text . itemBody) <> field "link" (return . maybe "" id . link . itemBody)) (return $ map (\f -> Item (fromString $ text f) f) $ filter ((== "characters") . category) favorites)
                  , pure $ listField "favorite-park-contents" (field "text" (return . text . itemBody) <> field "link" (return . maybe "" id . link . itemBody)) (return $ map (\f -> Item (fromString $ text f) f) $ filter ((== "park-contents") . category) favorites)
                      ]
                getResourceBody
                    >>= applyAsTemplate disneyExperienceSummaryCtx
                    >>= loadAndApplyTemplate rootTemplate disneyExperienceSummaryCtx
                    >>= modifyExternalLinkAttr
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
