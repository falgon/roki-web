{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
module Hakyll.Web.Feed.Extra (
    FeedConfiguration (..)
  , renderRss
  , renderAtom
) where

import           Data.FileEmbed              (makeRelativeToProject)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.Util.String     (replaceAll)
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.List
import           System.FilePath             ((</>))

data FeedConfiguration = FeedConfiguration {
    feedTitle       :: String
  , feedWebRoot     :: String
  , feedBlogName    :: String
  , feedDescription :: String
  , feedAuthorName  :: String
  , feedAuthorEmail :: String
  } deriving (Show, Eq)

rssTemplate :: Template
rssTemplate =
    $(makeRelativeToProject ("contents" </> "templates" </> "blog" </> "rss" </> "rss.xml")
        >>= embedTemplate)

rssItemTemplate :: Template
rssItemTemplate =
    $(makeRelativeToProject ("contents" </> "templates" </> "blog" </> "rss" </> "rss-item.xml")
        >>= embedTemplate)

atomTemplate :: Template
atomTemplate =
    $(makeRelativeToProject ("contents" </> "templates" </> "blog" </> "atom" </> "atom.xml")
        >>= embedTemplate)

atomItemTemplate :: Template
atomItemTemplate =
    $(makeRelativeToProject ("contents" </> "templates" </> "blog" </> "atom" </> "atom-item.xml")
        >>= embedTemplate)

renderFeed :: Template
    -> Template
    -> FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)
renderFeed feedTpl itemTpl config itemContext items = do
    protectedItems <- mapM (applyFilter protectCDATA) items
    body <- makeItem =<< applyTemplateList itemTpl itemContext' protectedItems
    applyTemplate feedTpl feedContext body
    where
        applyFilter :: (Monad m,Functor f) => (String -> String) -> f String -> m (f String)
        applyFilter tr str = return $ fmap tr str
        protectCDATA :: String -> String
        protectCDATA = replaceAll "]]>" (const "]]&gt;")

        itemContext' = mconcat
            [ itemContext
            , constField "webroot" (feedWebRoot config)
            , constField "authorName"  (feedAuthorName config)
            , constField "authorEmail" (feedAuthorEmail config)
            ]

        feedContext = mconcat
            [ bodyField  "body"
            , constField "title" (feedTitle config)
            , constField "webroot" (feedWebRoot config)
            , constField "blog-title" (feedBlogName config)
            , constField "description" (feedDescription config)
            , constField "authorName"  (feedAuthorName config)
            , constField "authorEmail" (feedAuthorEmail config)
            , urlField   "url"
            , updatedField
            , missingField
            ]

        updatedField = field "updated" $ const $ case items of
            [] -> return "Unknown"
            (x:_) -> unContext itemContext' "updated" [] x >>= \case
                StringField s -> return s
                _ -> fail "Hakyll.Web.Feed.Extra.renderFeed: Internal error"


renderRssWithTemplates :: Template
    -> Template
    -> FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)
renderRssWithTemplates feedTemplate itemTemplate config context = renderFeed
    feedTemplate itemTemplate config
    (makeItemContext "%a, %d %b %Y %H:%M:%S UT" context)

renderAtomWithTemplates :: Template
    -> Template
    -> FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)
renderAtomWithTemplates feedTemplate itemTemplate config context = renderFeed
    feedTemplate itemTemplate config
    (makeItemContext "%Y-%m-%dT%H:%M:%SZ" context)

makeItemContext :: String -> Context a -> Context a
makeItemContext fmt context = mconcat
    [context, dateField "published" fmt, dateField "updated" fmt]

renderRss :: FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)
renderRss = renderRssWithTemplates rssTemplate rssItemTemplate

renderAtom :: FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)
renderAtom = renderAtomWithTemplates atomTemplate atomItemTemplate
