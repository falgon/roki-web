module Rules.Blog.Search (
    build
) where

import           Control.Monad.Reader (asks, lift)
import           Hakyll
import           System.FilePath      ((</>))

import           Config.Blog          (BlogConfig (..))
import           Config.Program       (tmBlogRoot)
import           Config.Site          (defaultTimeLocale', timeZoneJST)
import           Contexts.Field       (searchBoxResultField)
import           Rules.Blog.Type
import           Rules.Blog.Utils     (appendFooter)
import           Utils                (absolutizeUrls, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome   as FA

build :: FA.FontAwesomeIcons
    -> Context String
    -> BlogConfReader m Rules ()
build faIcons ctx = do
    t <- asks blogName
    lift $ create [fromFilePath (t </> "search.html")] $ do
        route idRoute
        compile $
            makeItem mempty
                >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "default.html") (searchBoxResultField <> ctx)
                >>= absolutizeUrls
                >>= appendFooter t defaultTimeLocale' timeZoneJST
                >>= modifyExternalLinkAttr
                >>= relativizeUrls
                >>= FA.render faIcons
