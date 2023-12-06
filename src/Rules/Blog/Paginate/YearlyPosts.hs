module Rules.Blog.Paginate.YearlyPosts (
    build
) where

import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans       (MonadTrans (..))
import           Hakyll

import           Archives                  (YearlyArchives, archivesRules)
import           Config.Blog               (BlogConfig (..))
import           Rules.Blog.ListPage       (ListPageOpts, listPage)
import           Rules.Blog.Paginate.Utils
import           Rules.Blog.Type
import qualified Vendor.FontAwesome        as FA

build :: FA.FontAwesomeIcons
    -> Tags
    -> ListPageOpts
    -> BlogConfReader Rules Rules YearlyArchives
build faIcons tags listPageOpts = do
    yearlyArchives <- asks blogYearlyArchivesBuilder >>= lift
    grouper' <- grouper
    makeId' <- makeId blogYearlyPagePath
    lift $ archivesRules yearlyArchives $ \year pat ->
        let title' = title "Yearly" year
        in buildPaginateWith grouper' pat (makeId' year)
            >>= listPage (Just title') faIcons tags listPageOpts
    pure yearlyArchives
