{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Rules.DisneyExperienceSummary (rules) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.Trans    (MonadTrans (..))
import           Data.List              (sortBy)
import           Data.Ord               (comparing)
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import           Dhall                  (FromDhall, Generic, auto, input)
import           Hakyll
import           System.FilePath        (joinPath, (</>))
import           System.FilePath.Posix  (takeBaseName)

import           Config                 (contentsRoot, readerOptions)
import           Contexts               (siteCtx)
import           Media.SVG              (mermaidTransform)
import           Rules.PageType
import           Text.Pandoc.Walk       (walkM)
import           Utils                  (mconcatM, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome     as FA

data Favorite = Favorite {
    text     :: String
  , category :: String
  , link     :: Maybe String
  } deriving (Generic, Show)

instance FromDhall Favorite

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

filterFavoritesByCategory :: [Favorite] -> String -> [String]
filterFavoritesByCategory favorites cat = map text $ filter ((== cat) . category) favorites

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
                let works = filterFavoritesByCategory favorites "works"
                    characters = filterFavoritesByCategory favorites "characters"
                    parkContents = filterFavoritesByCategory favorites "park-contents"
                disneyExperienceSummaryCtx <- mconcatM [
                    pure $ constField "title" "Ponchi's Disney Journey"
                  , pure $ constField "font_path" "../fonts/waltograph42.otf"
                  , pure siteCtx
                  , pure defaultContext
                  , constField "about-body"
                        <$> loadSnapshotBody aboutIdent disneyExperienceSummarySnapshot
                  , pure $ listField "disney-logs" (metadataField <> bodyField "log-body") (return disneyLogs)
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
