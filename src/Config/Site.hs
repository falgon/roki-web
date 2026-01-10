{-# LANGUAGE OverloadedStrings #-}
module Config.Site (
    siteName
  , baseUrl
  , timeZoneJST
  , defaultTimeLocale'
  , GSuite (..)
  , gSuiteConf
) where

import           Data.Time.Format    (TimeLocale (..), defaultTimeLocale)
import           Data.Time.LocalTime (TimeZone (..))

siteName :: String
siteName = "roki.dev"

baseUrl :: String
baseUrl = "https://" <> siteName

timeZoneJST :: TimeZone
timeZoneJST = TimeZone (9 * 60) False "JST"

defaultTimeLocale' :: TimeLocale
defaultTimeLocale' = defaultTimeLocale {
    knownTimeZones = knownTimeZones defaultTimeLocale <> [timeZoneJST]
  }

data GSuite = GSuite {
    gCxPrefix      :: String
  , gSiteVerifyKey :: String
  } deriving Show

gSuiteConf :: GSuite
gSuiteConf = GSuite {
    gCxPrefix = "002573853708615501531"
  , gSiteVerifyKey = "13X5cycw11yFEsfZrhsQ0m_cSI90r7HucdErNDQ8Za8"
  }
