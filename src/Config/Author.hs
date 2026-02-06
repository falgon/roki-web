{-# LANGUAGE OverloadedStrings #-}
module Config.Author (
    authorName
  , authorJobTitle
  , authorGithub
  , authorTwitter
  , authorStackOverflow
  , authorDescriptionEn
  , authorDescriptionJa
) where

-- | 著者名
authorName :: String
authorName = "Roki"

-- | 職業
authorJobTitle :: String
authorJobTitle = "Software Engineer"

-- | GitHubアカウント
authorGithub :: String
authorGithub = "falgon"

-- | Twitterアカウント
authorTwitter :: String
authorTwitter = "roki_r7"

-- | Stack OverflowユーザーID
authorStackOverflow :: String
authorStackOverflow = "8345717"

-- | 説明文（英語）
authorDescriptionEn :: String
authorDescriptionEn = "Haskell enthusiast and Disney data analyst"

-- | 説明文（日本語）
authorDescriptionJa :: String
authorDescriptionJa = "Haskell愛好家、Disneyデータアナリスト"
