{-# LANGUAGE OverloadedStrings #-}

module Data.Disney.ExperienceSpec (spec) where

import           Data.Aeson             (decode, encode)
import           Data.Disney.Experience
import           Data.Time              (fromGregorian)
import           Test.Hspec

spec :: Spec
spec = do
    describe "SNSLinks" $ do
        it "SNSLinksをJSONにエンコード/デコードできる" $ do
            let snsLinks1 = SNSLinks
                    { youtube   = ["https://youtube.com/1"]
                    , instagram = ["https://instagram.com/1"]
                    , x         = ["https://x.com/1"]
                    , note      = ["https://note.com/1"]
                    }
            let encoded = encode snsLinks1
            decode encoded `shouldBe` Just snsLinks1

        it "空のSNSLinksをJSONにエンコード/デコードできる" $ do
            let snsLinks2 = SNSLinks
                    { youtube   = []
                    , instagram = []
                    , x         = []
                    , note      = []
                    }
            let encoded = encode snsLinks2
            decode encoded `shouldBe` Just snsLinks2

    describe "ExperienceRecord" $ do
        it "ExperienceRecordをJSONにエンコード/デコードできる" $ do
            let record = ExperienceRecord
                    { title       = "初めてのTDL"
                    , date        = fromGregorian 2024 1 1
                    , disneyTags  = ["TDL", "TDS"]
                    , snsLinks    = SNSLinks
                        { youtube   = ["https://youtube.com/1"]
                        , instagram = []
                        , x         = ["https://x.com/1"]
                        , note      = []
                        }
                    , aiGenerated = False
                    }
            let encoded = encode record
            decode encoded `shouldBe` Just record

        it "AI生成フラグがTrueのExperienceRecordをエンコード/デコードできる" $ do
            let record = ExperienceRecord
                    { title       = "AI生成記事"
                    , date        = fromGregorian 2024 12 1
                    , disneyTags  = ["TDL"]
                    , snsLinks    = SNSLinks [] [] [] []
                    , aiGenerated = True
                    }
            let encoded = encode record
            decode encoded `shouldBe` Just record

    describe "DailyCount" $ do
        it "DailyCountをJSONにエンコード/デコードできる" $ do
            let dailyCount = DailyCount
                    { dcDate  = fromGregorian 2024 1 1
                    , dcCount = 3
                    }
            let encoded = encode dailyCount
            decode encoded `shouldBe` Just dailyCount

        it "カウントが0のDailyCountをエンコード/デコードできる" $ do
            let dailyCount = DailyCount
                    { dcDate  = fromGregorian 2024 1 1
                    , dcCount = 0
                    }
            let encoded = encode dailyCount
            decode encoded `shouldBe` Just dailyCount

    describe "TimeSeriesData" $ do
        it "TimeSeriesDataをJSONにエンコード/デコードできる" $ do
            let timeSeriesData = TimeSeriesData
                    { daily =
                        [ DailyCount (fromGregorian 2024 1 1) 2
                        , DailyCount (fromGregorian 2024 1 2) 1
                        ]
                    }
            let encoded = encode timeSeriesData
            decode encoded `shouldBe` Just timeSeriesData

        it "空のTimeSeriesDataをエンコード/デコードできる" $ do
            let timeSeriesData = TimeSeriesData { daily = [] }
            let encoded = encode timeSeriesData
            decode encoded `shouldBe` Just timeSeriesData

    describe "TagCount" $ do
        it "TagCountをJSONにエンコード/デコードできる" $ do
            let tagCount = TagCount
                    { tcTag   = "TDL"
                    , tcCount = 45
                    }
            let encoded = encode tagCount
            decode encoded `shouldBe` Just tagCount

    describe "TagStats" $ do
        it "TagStatsをJSONにエンコード/デコードできる" $ do
            let tagStats1 = TagStats
                    { tags =
                        [ TagCount "TDL" 45
                        , TagCount "TDS" 30
                        , TagCount "DHM" 10
                        ]
                    }
            let encoded = encode tagStats1
            decode encoded `shouldBe` Just tagStats1

        it "空のTagStatsをエンコード/デコードできる" $ do
            let tagStats2 = TagStats { tags = [] }
            let encoded = encode tagStats2
            decode encoded `shouldBe` Just tagStats2

    describe "VisualizationData" $ do
        it "VisualizationDataをJSONにエンコード/デコードできる" $ do
            let vizData = VisualizationData
                    { timeSeries = TimeSeriesData
                        { daily = [DailyCount (fromGregorian 2024 1 1) 2]
                        }
                    , tagStats = TagStats
                        { tags = [TagCount "TDL" 45, TagCount "TDS" 30]
                        }
                    , yearlyTimeSeries = [YearData 2024 [DailyCount (fromGregorian 2024 1 1) 2]]
                    }
            let encoded = encode vizData
            decode encoded `shouldBe` Just vizData

        it "空のVisualizationDataをエンコード/デコードできる" $ do
            let vizData = VisualizationData
                    { timeSeries = TimeSeriesData { daily = [] }
                    , tagStats   = TagStats { tags = [] }
                    , yearlyTimeSeries = []
                    }
            let encoded = encode vizData
            decode encoded `shouldBe` Just vizData
