module Rules.Blog.Feed.Atom (
    build
) where

import           Hakyll
import qualified Hakyll.Web.Feed.Extra as FE

import           Rules.Blog.Feed.Utils
import           Rules.Blog.Type

build :: Snapshot
    -> Context String
    -> BlogConfReader n Rules ()
build feedSSName ctx = buildFeed feedSSName ".xml" ctx FE.renderAtom
