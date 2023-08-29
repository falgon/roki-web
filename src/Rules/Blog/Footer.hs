module Rules.Blog.Footer (
    build
) where

import           Control.Monad        (forM_)
import           Control.Monad.Extra  (mconcatMapM)
import           Control.Monad.Reader (asks, lift)
import           Hakyll
import           System.FilePath      ((</>))

import           Archives             (MonthlyArchives, YearlyArchives,
                                       archivesMap)
import           Config.Blog          (BlogConfig (..))
import           Config.Program       (tmBlogRoot)
import           Contexts             (siteCtx)
import qualified Contexts.Blog        as BlogCtx
import           Contexts.Field       (yearMonthArchiveField)
import           Contexts.Field       (tagCloudField')
import           Rules.Blog.Type

build :: Tags
    -> YearlyArchives
    -> MonthlyArchives
    -> BlogConfReader m Rules ()
build tags y m = do
    footerPath <- asks $ fromFilePath . (<> "-footer.html") . blogName
    pen <- asks blogPageEntriesNum
    ep <- asks blogEntryPattern
    cs <- asks blogContentSnapshot
    pCtxForFooter <- BlogCtx.postCtx tags
    footerCtx <- mconcatMapM id [
        pure $ tagCloudField' "tag-cloud" tags
      , pure $ siteCtx
      , BlogCtx.footerAdditionalComponent
      ]
    lift $ forM_ (Nothing : map (Just . fst) (archivesMap y)) $ \year -> maybe id version year $
        create [footerPath] $
            compile $ do
                recent <- fmap (take pen) .  recentFirst =<<
                    loadAllSnapshots ep cs
                let ctx = mconcat [
                        listField "recent-posts" pCtxForFooter (pure recent)
                      , yearMonthArchiveField "archives" y m year
                      , footerCtx
                      ]
                makeItem mempty
                    >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "footer.html") ctx
                    >>= relativizeUrls
