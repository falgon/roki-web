module Rules.Blog.Index (
    build
) where

import           Control.Monad.Reader (asks, lift)
import           Hakyll
import           System.FilePath      ((</>))

import           Config.Blog          (BlogConfig (..))
import           Rules.Blog.ListPage
import           Rules.Blog.Type
import           Utils                (makePageIdentifier)
import qualified Vendor.FontAwesome   as FA

build :: FA.FontAwesomeIcons
    -> Tags
    -> ListPageOpts
    -> BlogConfReader m Rules ()
build faIcons tags opts = do
    makeId <- asks $ makePageIdentifier . (</> "index.html") . blogName
    f <- asks $ fmap . paginateEvery . blogPageEntriesNum
    ep <- asks blogEntryPattern
    lift $ listPage Nothing faIcons tags opts =<<
        let grouper = f . sortRecentFirst
        in buildPaginateWith grouper ep makeId

