{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Rules.DisneyExperienceSummary (rules) where

import           Control.Monad.Reader  (asks)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.List             (sortBy)
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

-- タグの色とリンクの定義
tagConfig :: [(String, (String, String))]
tagConfig = [
    ("FSH", ("#854454", "https://www.tokyodisneyresort.jp/hotel/fsh.html"))
  , ("DHM", ("#8A7501", "https://www.tokyodisneyresort.jp/hotel/dhm.html"))
  , ("TDH", ("#B95C00", "https://www.tokyodisneyresort.jp/hotel/tdh.html"))
  , ("DAH", ("#1A2B8F", "https://www.tokyodisneyresort.jp/hotel/dah.html"))
  , ("TSH", ("#C28A02", "https://www.tokyodisneyresort.jp/hotel/tsh.html"))
  , ("TDL", ("#CF2C72", "https://www.tokyodisneyresort.jp/tdl/"))
  , ("TDS", ("#017788", "https://www.tokyodisneyresort.jp/tds/"))
  ]

-- タグの色を取得
getTagColor :: String -> String
getTagColor tag = case lookup tag tagConfig of
    Just (color, _) -> color
    Nothing         -> "#363636" -- Bulma is-dark の色

-- タグのリンクを取得
getTagLink :: String -> String
getTagLink tag = case lookup tag tagConfig of
    Just (_, tagLink) -> tagLink
    Nothing           -> "#"

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
disneyTagsField :: Context String
disneyTagsField = listFieldWith "disney-tags-list" tagCtx $ \item -> do
    mTags <- getMetadataField (itemIdentifier item) "disney-tags"
    case mTags of
        Just tagsStr -> return $ map (\tag -> Item (fromString tag) tag) $ filter (not . null) $ map trimMeta (splitAll "," tagsStr)
        Nothing      -> return []
  where
    tagCtx = field "name" (return . itemBody)
          <> field "color" (return . getTagColor . itemBody)
          <> field "link" (return . getTagLink . itemBody)

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
disneyLogCtx :: Context String
disneyLogCtx = mconcat
    [ metadataField
    , bodyField "log-body"
    , snsLinksField "youtube"
    , snsLinksField "instagram"
    , snsLinksField "x"
    , disneyTagsField
    ]

rules :: PageConfReader Rules ()
rules = do
    let items = disneyLogsPattern : map (fromList . (:[]))
            [ aboutIdent
            ]
    mapM_ (mdRule disneyExperienceSummarySnapshot) items
    faIcons <- asks pcFaIcons
    favorites <- lift $ preprocess loadDisneyFavorites
    lift $ do
        -- フォントファイルのコピー
        match (fromGlob $ joinPath [contentsRoot, "fonts", "*.otf"]) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler

        match disneyExperienceSummaryJPPath $ do
            route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
            compile $ do
                disneyLogs <- sortByNum <$> loadAllSnapshots disneyLogsPattern disneyExperienceSummarySnapshot
                disneyExperienceSummaryCtx <- mconcatM [
                    pure $ constField "title" "Ponchi's Disney Journey"
                  , pure $ constField "font_path" "../fonts/waltograph42.otf"
                  , pure $ listField "additional-css" (field "css" (return . itemBody)) (return $ map (\css -> Item (fromString css) css) ["./style/disney_experience_summary_only.css"])
                  , pure siteCtx
                  , pure defaultContext
                  , constField "about-body"
                        <$> loadSnapshotBody aboutIdent disneyExperienceSummarySnapshot
                  , pure $ listField "disney-logs" disneyLogCtx (return disneyLogs)
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
            (fromFilePath $ joinPath ["disney_experience_summary", "index.html"], joinPath ["/", "disney_experience_summary", "jp.html"])
          ]
    where
        disneyExperienceSummarySnapshot = "disneyExperienceSummarySS"
        disneyExperienceSummaryJPPath = fromGlob $ joinPath [contentsRoot, "pages", "disney_experience_summary", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"]
