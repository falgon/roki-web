module Rules.Blog.Type (
    BlogConfReader
) where

import           Config.Blog          (BlogConfig)
import           Control.Monad.Reader (ReaderT)

type BlogConfReader m = ReaderT (BlogConfig m)
