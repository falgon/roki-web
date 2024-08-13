{-# LANGUAGE BangPatterns, DeriveGeneric, DerivingStrategies,
             DuplicateRecordFields, LambdaCase, OverloadedStrings, QuasiQuotes,
             TemplateHaskell, TypeFamilies #-}
module Config.Contributions (
    reqGitHubPinnedRepo,
    renderProjectsList,
    renderContributionsTable,
    GetPinnedReposUserPinnedItemsNodes
) where

import           Control.Monad         (forM_)
import           Control.Monad.Fix     (fix)
import qualified Data.ByteString.UTF8  as BU
import           Data.Functor.Identity (Identity)
import           Data.Morpheus.Client  (declareLocalTypesInline, fetch, raw)
import           Data.String           (IsString (..))
import           Data.Text.Lazy        as TL
import           Dhall                 (FromDhall, Generic, Natural, auto,
                                        input)
import           Lucid.Base            (HtmlT, renderText)
import           Lucid.Html5
import           Network.HTTP.Req
import           Network.URI           (URI)
import           System.Environment    (lookupEnv)
import           System.FilePath       ((</>))

import           Data.Maybe            (fromJust)

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

declareLocalTypesInline "./tools/github/schema.docs.graphql"
    [raw|
        query GetPinnedRepos($user: String!) {
            user(login: $user) {
                pinnedItems(types: REPOSITORY, first: 6) {
                    nodes {
                        ... on Repository {
                            __typename
                            url
                            name
                            description
                            stargazerCount
                            languages(orderBy: {field: SIZE, direction: DESC}, first: 1) {
                                nodes {
                                    name
                                    color
                                }
                            }
                        }
                    }
                }
            }
        }
    |]

reqGitHubPinnedRepo :: BU.ByteString -> IO [Project]
reqGitHubPinnedRepo token = fetch jsonRes (GetPinnedReposArgs "falgon") >>= \case
    Left x -> print x >> pure []
    Right (GetPinnedRepos (Just x)) -> let GetPinnedReposUserPinnedItems (Just ns) = pinnedItems x in
        print (Prelude.map fromJust ns) >> pure []
    _ -> pure []
    where
        jsonRes b = runReq defaultHttpConfig $ responseBody
            <$> req POST (https "api.github.com" /: "graphql") (ReqBodyLbs b) lbsResponse headers
        headers = mconcat [
            header "Content-Type" "application/json"
          , header "User-Agent" "roki-web"
          , oAuth2Bearer token
          ]

renderProjectsList :: IO String
renderProjectsList = do
    ps <- maybe loadProjects (reqGitHubPinnedRepo . BU.fromString) =<< lookupEnv "GITHUB_TOKEN"
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
            tbody_ $ ($ (cs, 1 :: Int)) $ fix $ \f (cs', !i) -> case cs' of
                [] -> return mempty :: HtmlT Identity ()
                (c:cs'') -> do
                    tr_ $ do
                        td_ $ fromString $ show i
                        td_ $ a_ [href_ (fromString $ link c)] $ fromString $ text c
                        td_ $ div_ [class_ "tag is-success is-light"] $ fromString $ genre c
                        td_ $ let c' = date c in
                            fromString (show (yyyy c') </> show (mm c') </> show (dd c'))
                    f (cs'', succ i)
