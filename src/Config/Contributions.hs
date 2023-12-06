{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings #-}
module Config.Contributions (
    renderProjectsList,
    renderContributionsTable
) where

import           Control.Monad         (forM_)
import           Control.Monad.Fix     (fix)
import           Data.Functor.Identity (Identity)
import           Data.String           (IsString (..))
import qualified Data.Text.Lazy        as TL
import           Dhall                 (FromDhall, Generic, Natural, auto,
                                        input)
import           Lucid.Base            (HtmlT, renderText)
import           Lucid.Html5
import           System.FilePath       ((</>))

data Date = Date { yyyy :: Natural, mm :: Natural, dd :: Natural }
    deriving (Generic, Show)

instance FromDhall Date

data Project = Project {
    projName :: String
  , lang     :: String
  , summary  :: String
  , projLink :: String
  } deriving (Generic, Show)

instance FromDhall Project

data Contribute = Contribute {
    text  :: String
  , date  :: Date
  , link  :: String
  , genre :: String
  } deriving (Generic, Show)

instance FromDhall Contribute

loadProjects :: IO [Project]
loadProjects = input auto "./contents/config/contributions/Projects.dhall"

loadContributes :: IO [Contribute]
loadContributes = input auto "./contents/config/contributions/Contributions.dhall"

renderProjectsList :: IO String
renderProjectsList = do
    ps <- loadProjects
    return $ TL.unpack $ renderText $
        dl_ $ forM_ ps $ \p -> do
            dt_ [class_ "title is-4"] $ do
                a_ [href_ $ fromString $ projLink p] $ fromString $ projName p
                span_ [class_ "ml-2 tag is-success is-light"] $ fromString $ lang p
            dd_ [class_ "mb-6"] $ fromString $ summary p

renderContributionsTable :: IO String
renderContributionsTable = do
    cs <- loadContributes
    return $ TL.unpack $ renderText $ div_ [id_ "contributions_table"] $
        table_ [class_ "table is-fullwidth is-hoverable"] $ do
            thead_ $ tr_ $ do
                th_ $ abbr_ [title_ "Index"] "#"
                th_ $ abbr_ [title_ "Contents"] "Contents"
                th_ $ abbr_ [title_ "Genre"] "Genre"
                th_ $ abbr_ [title_ "Date"] "Date"
            tbody_ $ ($ (cs, 1 :: Int)) $ fix $ \f (cs', !i) ->
                if null cs' then return mempty :: HtmlT Identity () else let c = head cs' in do
                    tr_ $ do
                        td_ $ fromString $ show i
                        td_ $ a_ [href_ (fromString $ link c)] $ fromString $ text c
                        td_ $ div_ [class_ "tag is-success is-light"] $ fromString $ genre c
                        td_ $ let c' = date c in
                            fromString (show (yyyy c') </> show (mm c') </> show (dd c'))
                    f (tail cs', succ i)
