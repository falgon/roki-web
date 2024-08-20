module Rules.Blog.EachPostSeries (
    eachPostsSeries
) where

import           Control.Monad        (forM_, (>=>))
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans  (MonadTrans (..))
import           Data.Maybe           (catMaybes)
import           Hakyll

import           Config.Blog          (BlogConfig (..))
import           Rules.Blog.Type

eachPostsSeries :: (Context String -> Rules ()) -> BlogConfReader m Rules ()
eachPostsSeries rules = do
    postIDs <- asks blogEntryPattern >>= lift . (getMatches >=> sortChronological)
    forM_ (zip3 postIDs (nextPosts postIDs) (prevPosts postIDs)) $ \(pID, np, pp) -> do
        pageTitleOf' <- asks $ pageTitleOf . blogPrevNextTitleMaxNum
        lift $ create [pID] $
            rules $ mconcat $ catMaybes [
                field "previousPageUrl" . pageUrlOf <$> pp
              , field "previousPageTitle" . pageTitleOf' <$> pp
              , field "previousPageDate" . pageDateOf <$> pp
              , field "nextPageUrl" . pageUrlOf <$> np
              , field "nextPageTitle" . pageTitleOf' <$> np
              , field "nextPageDate" . pageDateOf <$> np
              ]
    where
        nextPosts postIDs = drop 1 $ map Just postIDs ++ [Nothing]
        prevPosts postIDs = Nothing : map Just postIDs
        pageTitleOf titleMax i = const $ getMetadataField i "title"
            >>= maybe (fail "no 'title' field")
                (\t -> return $ if length t > titleMax then take titleMax t <> "..." else t)
        pageUrlOf i = const (getRoute i >>= maybe (fail "no route") (return . toUrl))
        pageDateOf i = const $
            getMetadataField i "date"
                >>= maybe (fail "no 'date' field") (return . map (\x -> if x == '-' then '/' else x))
