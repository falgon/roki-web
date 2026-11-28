module Performance.PatternsSpec (spec) where

import           Config.Blogs.Utils            (entryFilesPattern, entryPattern)
import           Config.RegexUtils             (dd, mm, yyyy)
import           Control.Monad                 (forM_)
import           Hakyll                        (Identifier, Pattern,
                                                fromFilePath, fromRegex,
                                                matches, setVersion)
import           Rules.DisneyExperienceSummary (disneyConfigPath,
                                                disneyLogsPattern)
import           Rules.Resume                  (resumeCareerPattern)
import           Rules.TopPage                 (contributionsTypeConfigPath)
import           System.FilePath               (joinPath)
import           Test.Hspec

spec :: Spec
spec = describe "guarded resource patterns" $ do
    forM_ ["roki.log", "roki.diary"] $ \blogName -> do
        let root = "contents/" <> blogName <> "/"
            paths = map (root <>)
                [ "2024/01/15/post/index.md"
                , "2024/1/5/post/index.md"
                , "2024/01/15/post/nested/index.md"
                , "2024/01/15/post/assets/nested/image.png"
                , "2024/01/15/post/indexXmd"
                , "2024/01/15/post/other.md"
                , "2024/00/15/post/index.md"
                , "2024/13/15/post/index.md"
                , "2024/01/32/post/index.md"
                , "0999/01/15/post/index.md"
                , "2024/01/15/index.md"
                , "2024/01/15//index.md"
                , "post/index.md"
                ] <> ["contents/rokiXlog/2024/01/15/post/index.md"]
            legacy suffix = "(^" <> joinPath
                ["contents", blogName, yyyy, mm, dd, ".+", suffix] <> "$)"
        equivalent (blogName <> " posts") (entryPattern blogName) (legacy "index\\.md") paths
        equivalent (blogName <> " assets") (entryFilesPattern blogName) (legacy ".+") paths

    equivalent "Disney logs" disneyLogsPattern
        "(^contents/disney_experience_summary/logs/[0-9]+.md$)|(^contents/disney_experience_summary/logs/[0-9]+/index.md$)"
        [ "contents/disney_experience_summary/logs/1.md"
        , "contents/disney_experience_summary/logs/123/index.md"
        , "contents/disney_experience_summary/logs/01.md"
        , "contents/disney_experience_summary/logs/1Xmd"
        , "contents/disney_experience_summary/logs/1/indexXmd"
        , "contents/disney_experience_summary/logs/a.md"
        , "contents/disney_experience_summary/logs/1/other.md"
        , "contents/disney_experience_summary/logs/1/nested/index.md"
        ]

    equivalent "resume careers" resumeCareerPattern
        "(^contents/resume/career/[0-9]+\\.md$)"
        [ "contents/resume/career/1.md"
        , "contents/resume/career/001.md"
        , "contents/resume/career/1Xmd"
        , "contents/resume/career/a.md"
        , "contents/resume/career/1/index.md"
        ]

    forM_ [ ("contents/config/disney/", disneyConfigPath)
          , ("contents/config/contributions/Type/", contributionsTypeConfigPath)
          ] $ \(root, pattern) ->
        equivalent root pattern ("^" <> root <> ".+\\.dhall$") $ map (root <>)
            [ "Example.dhall"
            , "nested/Example.dhall"
            , "nested/deep/Example.dhall"
            , ".dhall"
            , "ExampleXdhall"
            , "Example.dhall.bak"
            , "Example.md"
            ]

equivalent :: String -> Pattern -> String -> [FilePath] -> Spec
equivalent name actual legacy paths =
    it ("preserves " <> name <> " matches, including versioned identifiers") $
        forM_ identifiers $ \identifier ->
            (identifier, matches actual identifier)
                `shouldBe` (identifier, matches (fromRegex legacy) identifier)
  where
    identifiers :: [Identifier]
    identifiers = [setVersion version (fromFilePath path)
        | path <- paths <> map ("node_modules/dependency/" <>) paths
            <> map ("prefix/" <>) paths <> ["node_modules/dependency/index.js", "contents/unrelated/file.md"]
        , version <- [Nothing, Just "snapshot"]
        ]
