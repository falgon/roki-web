module Config.SiteSpec (spec) where

import           Config.Site
import           Data.Time.Format    (TimeLocale (..))
import           Data.Time.LocalTime (timeZoneMinutes, timeZoneName,
                                      timeZoneSummerOnly)
import           Test.Hspec

spec :: Spec
spec = do
    describe "siteName" $ do
        it "returns the correct site name" $ do
            siteName `shouldBe` "roki.dev"

    describe "timeZoneJST" $ do
        it "has correct offset (9 hours = 540 minutes)" $ do
            timeZoneMinutes timeZoneJST `shouldBe` 540

        it "is not summer time" $ do
            timeZoneSummerOnly timeZoneJST `shouldBe` False

        it "has correct name" $ do
            timeZoneName timeZoneJST `shouldBe` "JST"

    describe "defaultTimeLocale'" $ do
        it "includes JST in known time zones" $ do
            let zones = knownTimeZones defaultTimeLocale'
            zones `shouldContain` [timeZoneJST]

    describe "gSuiteConf" $ do
        it "has correct gCxPrefix" $ do
            gCxPrefix gSuiteConf `shouldBe` "002573853708615501531"

        it "has correct gSiteVerifyKey" $ do
            gSiteVerifyKey gSuiteConf `shouldBe` "13X5cycw11yFEsfZrhsQ0m_cSI90r7HucdErNDQ8Za8"

    describe "GSuite" $ do
        it "supports Show instance" $ do
            show gSuiteConf `shouldContain` "GSuite"
            show gSuiteConf `shouldContain` "002573853708615501531"
