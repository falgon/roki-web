{-# LANGUAGE OverloadedStrings #-}
module Rules.DisneyExperienceSummary (rules) where

import           Control.Monad.Reader  (asks)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import           Data.String           (IsString (..))
import           Hakyll
import           System.FilePath       (joinPath, (</>))
import           System.FilePath.Posix (takeBaseName)

import           Config                (contentsRoot, readerOptions)
import           Contexts              (siteCtx)
import           Media.SVG             (mermaidTransform)
import           Rules.PageType
import           Text.Pandoc.Walk      (walkM)
import           Utils                 (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome    as FA

disneyExperienceSummaryRoot :: FilePath
disneyExperienceSummaryRoot = joinPath [contentsRoot, "disney_experience_summary"]

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

rules :: PageConfReader Rules ()
rules = do
    let items = [disneyLogsPattern]
    mapM_ (mdRule disneyExperienceSummarySnapshot) items
    faIcons <- asks pcFaIcons
    lift $ do
        -- フォントファイルのコピー
        match (fromGlob $ joinPath [contentsRoot, "fonts", "*.otf"]) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler

        match disneyExperienceSummaryJPPath $ do
            route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
            compile $ do
                disneyLogs <- sortByNum <$> loadAllSnapshots disneyLogsPattern disneyExperienceSummarySnapshot
                let disneyExperienceSummaryCtx = mconcat [
                        constField "title" "Ponchi’s Tokyo Disney Resort Journey"
                      , constField "font_path" "../../fonts/waltograph42.otf"
                      , listField "disney-logs" (metadataField <> bodyField "log-body") (return disneyLogs)
                      , siteCtx
                      , defaultContext
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
