module Rules.Blog.Footer (
    build
) where

import           Control.Monad        (forM_)
import           Control.Monad.Reader (asks, lift)
import           Hakyll
import           System.FilePath      ((</>))

import           Archives             (MonthlyArchives, YearlyArchives,
                                       archivesMap)
import           Config.Blog          (BlogConfig (..))
import           Config.Program       (tmBlogRoot)
import           Contexts             (siteCtx)
import qualified Contexts.Blog        as BlogCtx
import           Contexts.Field       (tagCloudField', yearMonthArchiveField)
import           Rules.Blog.Type
import           Utils                (mconcatM)

build :: Tags
    -> YearlyArchives
    -> MonthlyArchives
    -> BlogConfReader m Rules ()
build tags y m = do
    footerPath <- asks $ fromFilePath . (<> "-footer.html") . blogName
    recentPostsPath <- asks $ fromFilePath . (<> "-footer-recent-posts-cache") . blogName
    pen <- asks blogPageEntriesNum
    ep <- asks blogEntryPattern
    cs <- asks blogContentSnapshot
    pCtxForFooter <- BlogCtx.postCtx tags
    footerCtx <- mconcatM [
        pure $ tagCloudField' "tag-cloud" tags
      , pure $ siteCtx
      , BlogCtx.footerAdditionalComponent
      ]
    let recentPostsCompiler :: Compiler (Item String)
        recentPostsCompiler =
            makeItem =<< fmap (unlines . map (toFilePath . itemIdentifier) . take pen) . recentFirst
                =<< (loadAllSnapshots ep cs :: Compiler [Item String])
    lift $ do
        create [recentPostsPath] $
            compile recentPostsCompiler
        forM_ (Nothing : map (Just . fst) (archivesMap y)) $ \year -> maybe id version year $
            create [footerPath] $
                compile $ do
                    recentIds <- map fromFilePath . lines <$> loadBody recentPostsPath
                    recent <- mapM (`loadSnapshot` cs) recentIds
                    let ctx = mconcat [
                            listField "recent-posts" pCtxForFooter (pure recent)
                          , yearMonthArchiveField "archives" y m year
                          , footerCtx
                          ]
                    makeItem mempty
                        >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "footer.html") ctx
                        >>= relativizeUrls
