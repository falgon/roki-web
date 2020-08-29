{-# LANGUAGE OverloadedStrings #-}

module Archives (
    Archives (..)
  , YearlyArchives
  , MonthlyArchives
  , archivesRules
  , buildYearlyArchives
  , buildMonthlyArchives
) where

import           Control.Monad       (foldM, forM_)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Time.Format    (FormatTime, TimeLocale, formatTime)
import           Data.Time.LocalTime (TimeZone, utcToLocalTime)
import           Data.Tuple.Extra    (dupe, first, second)
import           Hakyll

data Archives k = Archives {
    archivesMap        :: [(k, [Identifier])]
  , archivesMakeId     :: k -> Identifier
  , archivesDependency :: Dependency
  }

type YearlyArchives = Archives String
type MonthlyArchives = Archives (String, String)

{-# INLINE fmtly #-}
fmtly :: FormatTime t => TimeLocale -> t -> String
fmtly locale time = formatTime locale "%Y" time

{-# INLINE fmtlm #-}
fmtlm :: FormatTime t => TimeLocale -> t -> String
fmtlm locale time = formatTime locale "%m" time

buildArchivesWith :: (MonadMetadata m, Ord k)
    => (Identifier -> m [k])
    -> Pattern
    -> (k -> Identifier)
    -> m (Archives k)
buildArchivesWith f pattern makeId = do
    ids <- getMatches pattern
    am  <- M.toList <$> foldM addToMap M.empty ids
    return $ Archives am makeId $ PatternDependency pattern (S.fromList ids)
    where
        addToMap m i = do
            ks <- f i
            return $
                M.unionWith (++) m $ M.fromList $ zip ks $ repeat [i]

archivesRules :: Archives a -> (a -> Pattern -> Rules ()) -> Rules ()
archivesRules archives rules = forM_ (archivesMap archives) $ \(key, identifiers) ->
    rulesExtraDependencies [archivesDependency archives] $ create [archivesMakeId archives key] $
        rules key $ fromList identifiers

buildYearlyArchives :: (MonadMetadata m, MonadFail m)
    => TimeLocale
    -> TimeZone
    -> Pattern
    -> (String -> Identifier)
    -> m YearlyArchives
buildYearlyArchives locale zone = buildArchivesWith $
    fmap (return . fmtly locale . utcToLocalTime zone) . getItemUTC locale

buildMonthlyArchives :: (MonadMetadata m, MonadFail m)
    => TimeLocale
    -> TimeZone
    -> Pattern
    -> ((String, String) -> Identifier)
    -> m MonthlyArchives
buildMonthlyArchives locale zone = buildArchivesWith $
    fmap ((:[]) . first (fmtly locale) . second (fmtlm locale) . dupe . utcToLocalTime zone) .
        getItemUTC locale
