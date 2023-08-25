module Rules.Blog.ListPage (
    listPage
  , ListPageOpts (..)
) where

import           Control.Monad.Extra (findM, ifM, mconcatMapM)
import           Data.Maybe          (isJust)
import           Hakyll
import           System.FilePath     ((</>))

import           Config              (defaultTimeLocale', timeZoneJST,
                                      tmBlogRoot)
-- TODO: remove
import           Contexts            (listCtx, postCtx)
-- TODO: remove
import           Contexts.Field      (tagCloudField')
import           Rules.Blog.Footer   (appendFooter)
import           Utils               (modifyExternalLinkAttr)
import qualified Vendor.FontAwesome  as FA

{-# INLINE pluginCtx #-}
pluginCtx :: MonadMetadata m => [Item a] -> String -> m (Context b)
pluginCtx posts pluginName = ifM
    (isJust <$> findM (fmap isJust . flip getMetadataField pluginName . itemIdentifier) posts)
    (return $ boolField pluginName (const True))
    (return mempty)

data ListPageOpts = ListPageOpts {
    lpName                                 :: Context String
  , lpTitle                                :: String
  , lpFont                                 :: Context String
  , lpDescription                          :: Context String
  , lpBeforeContentBodyAdditionalComponent :: Context String
  , lpHeaderAdditionalComponent            :: Context String
  , lpContentSnapshot                      :: Snapshot
  , lpGSuite                               :: Context String
  , lpIsPreview                            :: Bool
  }

-- MEMO: 専用の構造をつくって利用するコンテキストをそれ経由で渡す
listPage :: Maybe String
    -> FA.FontAwesomeIcons
    -> Tags
    -> ListPageOpts
    -> Paginate
    -> Rules ()
listPage title faIcons tags bc pgs = paginateRules pgs $ \pn pat -> do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAllSnapshots pat (lpContentSnapshot bc)
        pCtx <- mconcatMapM (pluginCtx posts) ["d3", "mathjs"]
        let blogCtx = mconcat [
                listField "posts" postCtx' (return posts)
              , pCtx
              , paginateContext pgs pn
              , maybe missingField (constField "title") title
              , listCtx $ lpIsPreview bc
              , tagCloudField' "tag-cloud" tags
              , lpName bc
              , lpFont bc
              , lpDescription bc
              , lpBeforeContentBodyAdditionalComponent bc
              , lpHeaderAdditionalComponent bc
              , lpGSuite bc
              ]
            postCtx' = mconcat [
                teaserField "teaser" (lpContentSnapshot bc)
              , postCtx (lpIsPreview bc) tags
              , lpName bc
              , pCtx
              ]

        makeItem ""
            >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "post-list.html") blogCtx
            >>= appendFooter (lpTitle bc) defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate (fromFilePath $ tmBlogRoot </> "default.html") blogCtx
            >>= modifyExternalLinkAttr
            >>= relativizeUrls
            >>= FA.render faIcons
