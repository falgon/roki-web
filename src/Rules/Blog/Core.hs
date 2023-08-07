{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog.Core (
    BlogConfig (..)
  , blogRules
) where
import           Control.Monad         (forM_)
import           Control.Monad.Except  (MonadError (..))
import           Control.Monad.Extra   (findM, ifM, mconcatMapM)
import           Data.Binary           (Binary)
import           Data.Maybe            (catMaybes, isJust)
import           Data.Time.Format      (TimeLocale, formatTime)
import           Data.Time.LocalTime   (TimeZone, utcToLocalTime)
import           Data.Typeable         (Typeable)
import           Hakyll                hiding (FeedConfiguration (..),
                                        renderAtom, renderRss)
import           Hakyll.Web.Feed.Extra
import           System.FilePath       ((</>))

import           Archives
import           Config
import           Config.Blog
import           Config.RegexUtils     (intercalateDir)
import           Contexts              (blogFontCtx, blogTitleCtx, gSuiteCtx,
                                        katexJsCtx, listCtx, postCtx, siteCtx,
                                        siteMapDateCtx)
import           Contexts.Field        (searchBoxResultField, tagCloudField',
                                        yearMonthArchiveField)
import           Utils                 (absolutizeUrls, makePageIdentifier,
                                        modifyExternalLinkAttr,
                                        sanitizeDisqusName)
import qualified Vendor.FontAwesome    as FA
import qualified Vendor.KaTeX          as KaTeX

appendFooter :: (Binary a, Typeable a, Semigroup a)
    => BlogConfig m
    -> TimeLocale
    -> TimeZone
    -> Item a
    -> Compiler (Item a)
appendFooter bc locale zone item = do
    utc <- fmap Just (getItemUTC locale (itemIdentifier item))
        `catchError` const (return Nothing)
    appendFooterWith (fmap (formatTime locale "%Y" . utcToLocalTime zone) utc) item
    where
        appendFooterWith y item' = do
            footer <- loadBody $ setVersion y $ fromFilePath (blogName bc <> "-footer.html")
            withItemBody (return . (<> footer)) item'

eachPostsSeries :: [Identifier] -> (Context String -> Rules ()) -> Rules ()
eachPostsSeries postIDs rules =
    forM_ (zip3 postIDs nextPosts prevPosts) $ \(pID, np, pp) -> create [pID] $
        rules $ mconcat $ catMaybes [
            field "previousPageUrl" . pageUrlOf <$> pp
          , field "previousPageTitle" . pageTitleOf <$> pp
          , field "previousPageDate" . pageDateOf <$> pp
          , field "nextPageUrl" . pageUrlOf <$> np
          , field "nextPageTitle" . pageTitleOf <$> np
          , field "nextPageDate" . pageDateOf <$> np
          ]
    where
        nextPosts = tail $ map Just postIDs ++ [Nothing]
        prevPosts = Nothing : map Just postIDs
        pageTitleOf i = const $ do
            t <- getMetadataField i "title"
            case t of
                Nothing -> fail "no 'title' field"
                Just t' -> return $ if length t' > 6 then take 6 t' <> "..." else t'
        pageUrlOf i = const (getRoute i >>= maybe (fail "no route") (return . toUrl))
        pageDateOf i = const $
            getMetadataField i "date"
                >>= maybe (fail "no 'date' field") (return . map (\x -> if x == '-' then '/' else x))

{-# INLINE pluginCtx #-}
pluginCtx :: MonadMetadata m => [Item a] -> String -> m (Context b)
pluginCtx posts pluginName = ifM
    (isJust <$> findM (fmap isJust . flip getMetadataField pluginName . itemIdentifier) posts)
    (return $ boolField pluginName (const True))
    (return mempty)

listPageRules :: Bool
    -> Maybe String
    -> FA.FontAwesomeIcons
    -> Tags
    -> BlogConfig m
    -> Paginate
    -> Rules ()
listPageRules isPreview title faIcons tags bc pgs = paginateRules pgs $ \pn pat -> do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAllSnapshots pat (blogContentSnapshot bc)
        pCtx <- mconcatMapM (pluginCtx posts) ["d3", "mathjs"]
        let blogCtx = listField "posts" postCtx' (return posts)
                <> pCtx
                <> paginateContext pgs pn
                <> maybe missingField (constField "title") title
                <> listCtx isPreview
                <> tagCloudField' "tag-cloud" tags
                <> blogTitleCtx (blogName bc)
                <> blogFontCtx (blogFont bc)
                <> constField "blog-description" (blogDescription bc)
                <> constField "before-content-body-additional-component" (blogBeforeContentBodyAdditional bc)
                <> constField "header-additional-component" (blogHeaderAdditional bc)
                <> gSuiteCtx bc
            postCtx' = teaserField "teaser" (blogContentSnapshot bc)
                <> postCtx isPreview tags
                <> blogTitleCtx (blogName bc)
                <> pCtx

        makeItem ""
            >>= loadAndApplyTemplate "contents/templates/blog/post-list.html" blogCtx
            >>= appendFooter bc defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" blogCtx
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons

blogRules :: Bool -> BlogConfig Rules -> FA.FontAwesomeIcons -> Rules ()
blogRules isPreview bc faIcons = do
    tags <- blogTagBuilder bc
    let postCtx' = postCtx isPreview tags
            <> tagCloudField' "tag-cloud" tags
            <> blogTitleCtx (blogName bc)
            <> blogFontCtx (blogName bc)
            <> constField "header-additional-component" (blogHeaderAdditional bc)
            <> constField "before-content-body-additional-component" (blogBeforeContentBodyAdditional bc)
            <> constField "blog-description" (blogDescription bc)
            <> gSuiteCtx bc
            <> if isPreview then katexJsCtx else mempty
        feedContent = blogName bc <> "-feed-content"

    -- each posts
    postIDs <- sortChronological =<< getMatches (blogEntryPattern bc)
    eachPostsSeries postIDs $ \s -> do
        route $ gsubRoute "contents/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompilerWith readerOptions (blogWriterOptions bc)
            >>= absolutizeUrls
            >>= saveSnapshot feedContent
            >>= (if isPreview then return else KaTeX.render)
            >>= saveSnapshot (blogContentSnapshot bc)
            >>= loadAndApplyTemplate "contents/templates/blog/post.html"
                (s <> postCtx' <> constField "disqus" (sanitizeDisqusName (blogName bc)))
            >>= appendFooter bc defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" postCtx'
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons

    match (blogEntryFilesPattern bc) $ do
        route $ gsubRoute "contents/" (const "")
        compile copyFileCompiler

    -- tag rules
    tagsRules tags $ \tag pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ blogTagPagesPath bc tag
            title = "Tagged posts: " <> tag
        in buildPaginateWith grouper pat makeId
            >>= listPageRules isPreview (Just title) faIcons tags bc

    -- yearly paginate
    yearlyArchives <- blogYearlyArchivesBuilder bc
    archivesRules yearlyArchives $ \year pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ blogYearlyPagePath bc year
            title = "Yearly posts: " <> year
        in buildPaginateWith grouper pat makeId
            >>= listPageRules isPreview (Just title) faIcons tags bc

    -- monthly paginate
    monthlyArchives <- blogMonthlyArchivesBuilder bc
    archivesRules monthlyArchives $ \key@(year, month) pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ blogMonthlyPagePath bc key
            title = "Monthly posts: " <> year </> month
        in buildPaginateWith grouper pat makeId
            >>= listPageRules isPreview (Just title) faIcons tags bc

    -- all tags
    let allTagsPagePath = blogName bc </> "tags" </> "index.html"
    listPageRules isPreview (Just "tags") faIcons tags bc =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier allTagsPagePath
        in buildPaginateWith grouper (blogEntryPattern bc) makeId

    -- the index page of tech blog
    listPageRules isPreview Nothing faIcons tags bc =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier (blogName bc </> "index.html")
        in buildPaginateWith grouper (blogEntryPattern bc) makeId

    -- footer
    let footerPath = fromFilePath (contentsRoot </> "templates" </> "blog" </> "footer.html")
    forM_ (Nothing:map (Just . fst) (archivesMap yearlyArchives)) $ \year -> maybe id version year $
        create [fromFilePath $ blogName bc <> "-footer.html"] $
            compile $ do
                recent <- fmap (take 5) . recentFirst =<<
                    loadAllSnapshots (blogEntryPattern bc) (blogContentSnapshot bc)
                let ctx = listField "recent-posts" (postCtx isPreview tags) (return recent)
                        <> tagCloudField' "tag-cloud" tags
                        <> yearMonthArchiveField "archives" yearlyArchives monthlyArchives year
                        <> siteCtx
                        <> constField "footer-additional-component" (blogFooterAdditional bc)
                makeItem ""
                    >>= loadAndApplyTemplate footerPath ctx
                    >>= relativizeUrls

    -- Atom Feed
    create [fromFilePath (blogName bc </> "feed" </> blogName bc <> ".xml")] $ do
        route idRoute
        compile $
            loadAllSnapshots (blogEntryPattern bc) feedContent
                >>= fmap (take 20) . recentFirst
                >>= renderAtom (blogAtomConfig bc) (bodyField "description" <> postCtx')

    -- RSS Feed
    create [fromFilePath (blogName bc </> "feed" </> blogName bc <> "-rss.xml")] $ do
        route idRoute
        compile $
            loadAllSnapshots (blogEntryPattern bc) feedContent
                >>= fmap (take 20) . recentFirst
                >>= renderRss (blogAtomConfig bc) (bodyField "description" <> postCtx')

    -- Search result page
    let rootTemplate = fromFilePath $
            intercalateDir [contentsRoot, "templates", "blog", "default.html"]
    create [fromFilePath (blogName bc </> "search.html")] $ do
        route idRoute
        compile $
            makeItem ""
                >>= loadAndApplyTemplate rootTemplate (searchBoxResultField <> postCtx')
                >>= absolutizeUrls
                >>= appendFooter bc defaultTimeLocale' timeZoneJST
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons

    -- Site map
    create [fromFilePath (blogName bc </> "sitemap.xml")] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots (blogEntryPattern bc) feedContent
            let hostCtx = constField "webroot" ("https://" <> siteName)
                sitemapCtx = hostCtx
                    <> blogTitleCtx (blogName bc)
                    <> listField "pages" (siteMapDateCtx <> hostCtx <> defaultContext) (return posts)
            makeItem ""
                >>= loadAndApplyTemplate
                    (fromFilePath $ intercalateDir [contentsRoot, "templates", "blog", "sitemap.xml"])
                        sitemapCtx
