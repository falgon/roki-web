module Rules.Blog.Sitemap (
    build
) where

import           Control.Monad.Extra  (mconcatMapM)
import           Control.Monad.Reader (asks, lift)
import           Hakyll
import           System.FilePath      ((</>))

import           Config               (tmBlogRoot)
import           Config.Blog          (BlogConfig (..))
import           Config.Site          (siteName)
import           Contexts             (siteMapDateCtx)
import qualified Contexts.Blog        as BlogCtx
import           Rules.Blog.Type

build :: Snapshot
    -> BlogConfReader m Rules ()
build feedSS = do
    sitemapXML <- asks $ fromFilePath . (</> xml) . blogName
    ep <- asks blogEntryPattern
    ctx <- mconcatMapM id [
        BlogCtx.title
      , pure hostCtx
      ]
    lift $ create [sitemapXML] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ep feedSS
            let sitemapCtx = mconcat [
                    ctx
                  , listField "pages" (mconcat [siteMapDateCtx, hostCtx, defaultContext]) (pure posts)
                  ]
            makeItem mempty
                >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> xml) sitemapCtx
    where
        hostCtx = constField "webroot" ("https://" <> siteName)
        xml = "sitemap.xml"
