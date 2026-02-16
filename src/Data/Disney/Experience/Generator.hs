{-# LANGUAGE OverloadedStrings #-}

-- | Disney体験記録のJSON生成パイプラインモジュール
-- このモジュールは、Hakyllコンテキストでディズニーログを集約し、
-- D3.js可視化用のJSONデータを生成する
module Data.Disney.Experience.Generator
    ( generateVisualizationData
    , aggregateDailyData
    , aggregateTagData
    , aggregateByYear
    , parseLogMetadata
    ) where

import           Control.Monad          (forM)
import           Data.Aeson             (encode)
import qualified Data.ByteString.Lazy   as BL
import           Data.Disney.Experience
import           Data.List              (foldl', sort)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (Day, defaultTimeLocale, parseTimeM,
                                         toGregorian)
import           Hakyll

-- | メタデータから体験記録をパースする
parseLogMetadata :: Metadata -> Maybe ExperienceRecord
parseLogMetadata meta = do
    titleStr <- lookupString "title" meta
    dateStr <- lookupString "date" meta
    date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr
    let tagsStr = fromMaybe "" $ lookupString "disney-tags" meta
        tags = filter (not . T.null) $ map T.strip $ T.splitOn "," $ T.pack tagsStr
        aiGeneratedByStr = fromMaybe "" $ lookupString "ai-generated-by" meta
        aiGeneratedBy = T.strip $ T.pack aiGeneratedByStr
        aiGeneratedStr = fromMaybe "false" $ lookupString "ai-generated" meta
        aiGenerated = (not $ T.null aiGeneratedBy) || T.toLower (T.pack aiGeneratedStr) == "true"

        -- SNSリンクの取得
        youtube = parseLinks $ fromMaybe "" $ lookupString "youtube" meta
        instagram = parseLinks $ fromMaybe "" $ lookupString "instagram" meta
        xLinks = parseLinks $ fromMaybe "" $ lookupString "x" meta
        note = parseLinks $ fromMaybe "" $ lookupString "note" meta

        snsLinks = SNSLinks
            { youtube = youtube
            , instagram = instagram
            , x = xLinks
            , note = note
            }

    return ExperienceRecord
        { title = T.pack titleStr
        , date = date
        , disneyTags = tags
        , snsLinks = snsLinks
        , aiGenerated = aiGenerated
        }

-- | カンマ区切りの文字列をリストにパース
parseLinks :: String -> [Text]
parseLinks str = filter (not . T.null) $ map T.strip $ T.splitOn "," $ T.pack str

-- | 日別データを集約
aggregateDailyData :: [ExperienceRecord] -> [DailyCount]
aggregateDailyData records =
    let dateCountMap = foldl' (\acc rec -> M.insertWith (+) (date rec) 1 acc) M.empty records
        sortedDates = sort $ M.keys dateCountMap
    in map (\d -> DailyCount d (dateCountMap M.! d)) sortedDates

-- | タグ別データを集約
aggregateTagData :: [ExperienceRecord] -> [TagCount]
aggregateTagData records =
    let tagCountMap = foldl' (\acc rec -> foldl' (\m tag -> M.insertWith (+) tag 1 m) acc (disneyTags rec)) M.empty records
        sortedTags = sort $ M.keys tagCountMap
    in map (\t -> TagCount t (tagCountMap M.! t)) sortedTags

-- | 年度別にデータを集約
aggregateByYear :: [ExperienceRecord] -> [YearData]
aggregateByYear records =
    let yearGrouped = M.fromListWith (++) [(getYear $ date rec, [rec]) | rec <- records]
        sortedYears = sort $ M.keys yearGrouped
    in map (\y -> YearData y (aggregateDailyData (yearGrouped M.! y))) sortedYears
  where
    getYear day = let (y, _, _) = toGregorian day in fromInteger y

-- | 可視化データを生成
generateVisualizationData :: [Item a] -> Compiler VisualizationData
generateVisualizationData items = do
    records <- forM items $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        return $ parseLogMetadata meta

    let validRecords = [rec | Just rec <- records]
        dailyData = aggregateDailyData validRecords
        tagData = aggregateTagData validRecords
        yearlyData = aggregateByYear validRecords

        timeSeriesData = TimeSeriesData { daily = dailyData }
        tagStatsData = TagStats { tags = tagData }

    return VisualizationData
        { timeSeries = timeSeriesData
        , tagStats = tagStatsData
        , yearlyTimeSeries = yearlyData
        }

-- | JSONファイルとして保存
saveVisualizationDataAsJSON :: VisualizationData -> FilePath -> IO ()
saveVisualizationDataAsJSON vizData filepath =
    BL.writeFile filepath (encode vizData)
