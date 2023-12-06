module Rules.Blog.Feed.RSS (
    build
) where

import           Hakyll
import qualified Hakyll.Web.Feed.Extra as FE

import           Rules.Blog.Feed.Utils
import           Rules.Blog.Type

build :: Snapshot
    -> Context String
    -> BlogConfReader n Rules ()
build feedSSName ctx = buildFeed feedSSName "-rss.xml" ctx FE.renderRss
