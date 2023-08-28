module Rules.Blog.Paginate.TaggedPosts (
    build
) where

import           Control.Monad.Trans       (MonadTrans (..))
import           Hakyll

import           Config.Blog               (BlogConfig (..))
import           Rules.Blog.ListPage       (ListPageOpts, listPage)
import           Rules.Blog.Paginate.Utils
import           Rules.Blog.Type
import qualified Vendor.FontAwesome        as FA

build :: FA.FontAwesomeIcons
    -> Tags
    -> ListPageOpts
    -> BlogConfReader n Rules ()
build faIcons tags listPageOpts = do
    grouper' <- grouper
    makeId' <- makeId blogTagPagesPath
    lift $ tagsRules tags $ \tag pat ->
        let title' = title "Tagged" tag
        in buildPaginateWith grouper' pat (makeId' tag)
            >>= listPage (Just title') faIcons tags listPageOpts
