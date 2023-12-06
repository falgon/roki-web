module Rules.Blog.Paginate.MonthlyPosts (
    build
) where

import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans       (MonadTrans (..))
import           Hakyll
import           System.FilePath           ((</>))

import           Archives                  (MonthlyArchives, archivesRules)
import           Config.Blog               (BlogConfig (..))
import           Rules.Blog.ListPage       (ListPageOpts, listPage)
import           Rules.Blog.Paginate.Utils
import           Rules.Blog.Type
import qualified Vendor.FontAwesome        as FA

build :: FA.FontAwesomeIcons
    -> Tags
    -> ListPageOpts
    -> BlogConfReader Rules Rules MonthlyArchives
build faIcons tags listPageOpts = do
    monthlyArchives <- asks blogMonthlyArchivesBuilder >>= lift
    grouper' <- grouper
    makeId' <- makeId blogMonthlyPagePath
    lift $ archivesRules monthlyArchives $ \key@(year, month) pat ->
        let title' = title "Monthly" $ year </> month
        in buildPaginateWith grouper' pat (makeId' key)
            >>= listPage (Just title') faIcons tags listPageOpts
    pure monthlyArchives
