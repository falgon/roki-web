module Config.TopPage (
    TopPageConfig (..)
  , topPageConfig
) where

data TopPageConfig = TopPageConfig {
    maxTitleNum    :: Int
  , postDateFormat :: String
  , noPostsAlt     :: String
  }

topPageConfig :: TopPageConfig
topPageConfig = TopPageConfig {
    maxTitleNum = 4
  , postDateFormat = "%Y%%2F%m%%2F%d" -- "%%2F" is URL encoded slash
  , noPostsAlt = "not yet"
  }
