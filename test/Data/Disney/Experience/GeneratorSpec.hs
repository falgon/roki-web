{-# LANGUAGE OverloadedStrings #-}

module Data.Disney.Experience.GeneratorSpec (spec) where

import           Data.Disney.Experience
import           Data.Disney.Experience.Generator
import           Data.Time                        (fromGregorian)
import           Test.Hspec

spec :: Spec
spec = do
    describe "aggregateDailyData" $ do
        it "日別データを正しく集約する" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 1) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 2) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateDailyData records
            result `shouldBe`
                [ DailyCount (fromGregorian 2024 1 1) 2
                , DailyCount (fromGregorian 2024 1 2) 1
                ]

        it "空のリストの場合は空を返す" $ do
            let result = aggregateDailyData []
            result `shouldBe` []

        it "同じ日付のデータを正しくカウントする" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateDailyData records
            result `shouldBe` [DailyCount (fromGregorian 2024 1 1) 3]

        it "日付順にソートされる" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 3) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 1) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 2) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateDailyData records
            result `shouldBe`
                [ DailyCount (fromGregorian 2024 1 1) 1
                , DailyCount (fromGregorian 2024 1 2) 1
                , DailyCount (fromGregorian 2024 1 3) 1
                ]

    describe "aggregateTagData" $ do
        it "タグ別データを正しく集約する" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL", "DHM"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 2) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 3) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateTagData records
            result `shouldBe`
                [ TagCount "DHM" 1
                , TagCount "TDL" 2
                , TagCount "TDS" 1
                ]

        it "空のリストの場合は空を返す" $ do
            let result = aggregateTagData []
            result `shouldBe` []

        it "複数タグを持つ記事を正しくカウントする" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL", "TDS", "DHM"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateTagData records
            result `shouldBe`
                [ TagCount "DHM" 1
                , TagCount "TDL" 1
                , TagCount "TDS" 1
                ]

        it "タグ名順にソートされる" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 2) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 3) ["DHM"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateTagData records
            result `shouldBe`
                [ TagCount "DHM" 1
                , TagCount "TDL" 1
                , TagCount "TDS" 1
                ]

        it "同じタグを持つ複数の記事を正しくカウントする" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 2) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 3) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateTagData records
            result `shouldBe` [TagCount "TDL" 3]

    describe "aggregateByYear" $ do
        it "年度別にデータを正しく集約する" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 1 2) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2023 12 31) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateByYear records
            result `shouldBe`
                [ YearData 2023 [DailyCount (fromGregorian 2023 12 31) 1]
                , YearData 2024 [DailyCount (fromGregorian 2024 1 1) 1, DailyCount (fromGregorian 2024 1 2) 1]
                ]

        it "空のリストの場合は空を返す" $ do
            let result = aggregateByYear []
            result `shouldBe` []

        it "単一年のデータを正しく処理する" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2024 6 15) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 12 31) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateByYear records
            result `shouldBe`
                [ YearData 2024
                    [ DailyCount (fromGregorian 2024 1 1) 1
                    , DailyCount (fromGregorian 2024 6 15) 1
                    , DailyCount (fromGregorian 2024 12 31) 1
                    ]
                ]

        it "複数年のデータを年順にソートする" $ do
            let records =
                    [ ExperienceRecord "記事1" (fromGregorian 2025 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事2" (fromGregorian 2023 1 1) ["TDS"] (SNSLinks [] [] [] []) False
                    , ExperienceRecord "記事3" (fromGregorian 2024 1 1) ["TDL"] (SNSLinks [] [] [] []) False
                    ]
            let result = aggregateByYear records
            length result `shouldBe` 3
            yearNumber (head result) `shouldBe` 2023
            yearNumber (result !! 1) `shouldBe` 2024
            yearNumber (result !! 2) `shouldBe` 2025
