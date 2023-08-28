module Rules.Blog.Feed.Utils (
    buildFeed
) where

import           Control.Monad.Reader  (asks)
import           Control.Monad.Trans   (MonadTrans (..))
import           Hakyll
import qualified Hakyll.Web.Feed.Extra as FE
import           System.FilePath       (joinPath)

import           Config.Blog           (BlogConfig (..))
import           Rules.Blog.Type

type Render = FE.FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

buildFeed :: Snapshot
    -> String
    -> Context String
    -> Render
    -> BlogConfReader n Rules ()
buildFeed feedSSName suffix ctx render = do
    t <- asks blogName
    feedTake <- asks $ take . blogFeedRecentNum
    ep <- asks blogEntryPattern
    fc <- asks blogFeedConfig
    lift $ create [fromFilePath (joinPath [t, "feed", t <> suffix])] $ do
        route idRoute
        compile $
            loadAllSnapshots ep feedSSName
                >>= fmap feedTake . recentFirst
                >>= render fc (bodyField "description" <> ctx)
