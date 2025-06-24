{-# LANGUAGE OverloadedStrings #-}
module Rules.DisneyResume (rules) where

import           Control.Monad.Reader          (asks)
import           Control.Monad.Trans           (MonadTrans (..))
import           Data.List                     (sortBy)
import           Data.Ord                      (comparing)
import           Data.String                   (IsString (..))
import           Hakyll
import           System.FilePath               (joinPath, (</>))
import           System.FilePath.Posix         (takeBaseName)

import           Config                        (contentsRoot, readerOptions)
import           Contexts                      (siteCtx)
import           Media.SVG                     (mermaidTransform)
import           Rules.PageType
import           Text.Pandoc.Walk              (walkM)
import           Utils                         (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome            as FA

disneyResumeRoot :: FilePath
disneyResumeRoot = joinPath [contentsRoot, "disney_resume"]

disneyLogsPattern :: Pattern
disneyLogsPattern = fromRegex $ mconcat
    [ "(^"
    , joinPath [disneyResumeRoot, "logs", "[0-9]+.md"]
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
    mapM_ (mdRule disneyResumeSnapshot) items
    faIcons <- asks pcFaIcons
    lift $ do
        -- フォントファイルのコピー
        match (fromGlob $ joinPath [contentsRoot, "fonts", "*.otf"]) $ do
            route $ gsubRoute "contents/" $ const ""
            compile copyFileCompiler
        
        match disneyResumeJPPath $ do
            route $ gsubRoute (contentsRoot </> "pages/") (const mempty)
            compile $ do
                disneyLogs <- sortByNum <$> loadAllSnapshots disneyLogsPattern disneyResumeSnapshot
                let disneyResumeCtx = mconcat [
                        constField "title" "ディズニーリゾートで遊んだ経歴"
                      , constField "font_path" "/fonts/waltograph42.otf"
                      , listField "logs" (metadataField <> bodyField "log-body") (return disneyLogs)
                      , siteCtx
                      , defaultContext
                      ]
                getResourceBody
                    >>= applyAsTemplate disneyResumeCtx
                    >>= loadAndApplyTemplate rootTemplate disneyResumeCtx
                    >>= modifyExternalLinkAttr
                    >>= relativizeUrls
                    >>= FA.render faIcons
        
        createRedirects [
            (fromFilePath $ joinPath ["disney_resume", "index.html"], joinPath ["/", "disney_resume", "jp.html"])
          ]
    where
        disneyResumeSnapshot = "disneyResumeSS"
        disneyResumeJPPath = fromGlob $ joinPath [contentsRoot, "pages", "disney_resume", "jp.html"]
        rootTemplate = fromFilePath $ joinPath [contentsRoot, "templates", "site", "default.html"] 