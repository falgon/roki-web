module Rules.Blog.Paginate.Utils (
    grouper
  , makeId
  , title
) where

import           Control.Monad.Reader (asks)
import           Hakyll

import           Config.Blog          (BlogConfig (..))
import           Rules.Blog.Type
import           Utils                (makePageIdentifier)

grouper :: (Monad m, MonadMetadata f, MonadFail f)
    => BlogConfReader n m ([Identifier] -> f [[Identifier]])
grouper = asks $
    flip (.) sortRecentFirst . fmap . paginateEvery . blogPageEntriesNum

makeId :: Monad n
    => (BlogConfig m -> a -> FilePath)
    -> BlogConfReader m n (a -> PageNumber -> Identifier)
makeId = fmap (makePageIdentifier .) . asks

title :: String
    -> String
    -> String
title s tag = unwords [s, "posts:", tag]

