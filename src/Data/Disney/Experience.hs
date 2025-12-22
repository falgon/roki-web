{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- | Disney体験記録のデータ構造を定義するモジュール
-- このモジュールは、Disney体験記録の型定義とJSON変換のためのインスタンスを提供する
module Data.Disney.Experience
    ( ExperienceRecord (..)
    , SNSLinks (..)
    , TimeSeriesData (..)
    , DailyCount (..)
    , TagStats (..)
    , TagCount (..)
    , VisualizationData (..)
    ) where

import           Data.Aeson   (FromJSON (..), ToJSON (..), object, toJSON,
                               withObject, (.:), (.=))
import           Data.Text    (Text)
import           Data.Time    (Day)
import           GHC.Generics (Generic)

-- | SNSリンクの構造
data SNSLinks = SNSLinks
    { youtube   :: [Text]
    , instagram :: [Text]
    , x         :: [Text]
    , note      :: [Text]
    } deriving (Eq, Show, Generic)

instance ToJSON SNSLinks where
    toJSON (SNSLinks yt ig xLinks nt) = object
        [ "youtube"   .= yt
        , "instagram" .= ig
        , "x"         .= xLinks
        , "note"      .= nt
        ]

instance FromJSON SNSLinks where
    parseJSON = withObject "SNSLinks" $ \v -> SNSLinks
        <$> v .: "youtube"
        <*> v .: "instagram"
        <*> v .: "x"
        <*> v .: "note"

-- | 体験記録の構造
data ExperienceRecord = ExperienceRecord
    { title       :: Text
    , date        :: Day
    , disneyTags  :: [Text]
    , snsLinks    :: SNSLinks
    , aiGenerated :: Bool
    } deriving (Eq, Show, Generic)

instance ToJSON ExperienceRecord where
    toJSON (ExperienceRecord t d tags sns ai) = object
        [ "title"        .= t
        , "date"         .= d
        , "disneyTags"   .= tags
        , "snsLinks"     .= sns
        , "aiGenerated"  .= ai
        ]

instance FromJSON ExperienceRecord where
    parseJSON = withObject "ExperienceRecord" $ \v -> ExperienceRecord
        <$> v .: "title"
        <*> v .: "date"
        <*> v .: "disneyTags"
        <*> v .: "snsLinks"
        <*> v .: "aiGenerated"

-- | 日別カウントの構造
data DailyCount = DailyCount
    { dcDate  :: Day
    , dcCount :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON DailyCount where
    toJSON (DailyCount d c) = object
        [ "date"  .= d
        , "count" .= c
        ]

instance FromJSON DailyCount where
    parseJSON = withObject "DailyCount" $ \v -> DailyCount
        <$> v .: "date"
        <*> v .: "count"

-- | 時系列データの構造
data TimeSeriesData = TimeSeriesData
    { daily :: [DailyCount]
    } deriving (Eq, Show, Generic)

instance ToJSON TimeSeriesData where
    toJSON (TimeSeriesData d) = object [ "daily" .= d ]

instance FromJSON TimeSeriesData where
    parseJSON = withObject "TimeSeriesData" $ \v -> TimeSeriesData
        <$> v .: "daily"

-- | タグ別カウントの構造
data TagCount = TagCount
    { tcTag   :: Text
    , tcCount :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON TagCount where
    toJSON (TagCount t c) = object
        [ "tag"   .= t
        , "count" .= c
        ]

instance FromJSON TagCount where
    parseJSON = withObject "TagCount" $ \v -> TagCount
        <$> v .: "tag"
        <*> v .: "count"

-- | タグ統計の構造
data TagStats = TagStats
    { tags :: [TagCount]
    } deriving (Eq, Show, Generic)

instance ToJSON TagStats where
    toJSON (TagStats ts) = object [ "tags" .= ts ]

instance FromJSON TagStats where
    parseJSON = withObject "TagStats" $ \v -> TagStats
        <$> v .: "tags"

-- | 可視化データの全体構造
data VisualizationData = VisualizationData
    { timeSeries :: TimeSeriesData
    , tagStats   :: TagStats
    } deriving (Eq, Show, Generic)

instance ToJSON VisualizationData where
    toJSON (VisualizationData ts tg) = object
        [ "timeSeries" .= ts
        , "tagStats"   .= tg
        ]

instance FromJSON VisualizationData where
    parseJSON = withObject "VisualizationData" $ \v -> VisualizationData
        <$> v .: "timeSeries"
        <*> v .: "tagStats"
