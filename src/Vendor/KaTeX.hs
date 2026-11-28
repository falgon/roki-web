{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Vendor.KaTeX (
    KaTeXRender
  , KaTeXRequest (..)
  , KaTeXResponse (..)
  , render
  , renderHtmlWithM
  , renderHtmlWith
  , renderKaTeXBatchWith
) where

import           Control.Monad          (unless)
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as BL
import           Data.Functor.Identity  (Identity (..), runIdentity)
import           Data.Maybe             (fromMaybe)
import           Hakyll
import qualified Text.HTML.TagSoup      as TS
import qualified Text.HTML.TagSoup.Tree as TT

import           Config                 (tagSoupOption)

type KaTeXRender = Item String -> Compiler (Item String)

data KaTeXRequest = KaTeXRequest
    { requestIndex       :: !Int
    , requestMath        :: !String
    , requestDisplayMode :: !Bool
    } deriving (Eq, Show)

data KaTeXResponse = KaTeXResponse
    { responseIndex :: !Int
    , responseHtml  :: !String
    } deriving (Eq, Show)

instance A.ToJSON KaTeXRequest where
    toJSON req = A.object
        [ "index" A..= requestIndex req
        , "math" A..= requestMath req
        , "displayMode" A..= requestDisplayMode req
        ]

instance A.FromJSON KaTeXRequest where
    parseJSON = A.withObject "KaTeXRequest" $ \obj ->
        KaTeXRequest
            <$> obj A..: "index"
            <*> obj A..: "math"
            <*> obj A..: "displayMode"

instance A.ToJSON KaTeXResponse where
    toJSON response = A.object
        [ "index" A..= responseIndex response
        , "html" A..= responseHtml response
        ]

instance A.FromJSON KaTeXResponse where
    parseJSON = A.withObject "KaTeXResponse" $ \obj ->
        KaTeXResponse
            <$> obj A..: "index"
            <*> obj A..: "html"

classes :: [(String, String)] -> [String]
classes = words . fromMaybe "" . lookup "class"

hasDisplayClass :: [(String, String)] -> Bool
hasDisplayClass = elem "display" . classes

hasMathClass :: [(String, String)] -> Bool
hasMathClass = elem "math" . classes

mathRequestFor :: Int -> TT.TagTree String -> Maybe KaTeXRequest
mathRequestFor index (TT.TagBranch _ as [TT.TagLeaf (TS.TagText math)])
    | hasMathClass as = Just $ KaTeXRequest index math (hasDisplayClass as)
mathRequestFor _ _ = Nothing

collectRequests :: [TT.TagTree String] -> [KaTeXRequest]
collectRequests = zipWith reindex [0..] . collectMath
  where
    reindex index req = req {requestIndex = index}

    collectMath = concatMap $ \case
        tag@(TT.TagBranch _ _ children) -> maybe (collectMath children) (:[]) $ mathRequestFor 0 tag
        _                               -> []

renderTreeWith :: [KaTeXResponse] -> [TT.TagTree String] -> Either String [TT.TagTree String]
renderTreeWith responses tree = do
    (remaining, rendered) <- goList responses tree
    unless (null remaining) $ Left "KaTeX batch response was longer than the request list"
    pure rendered
  where
    goList rs [] = Right (rs, [])
    goList rs (tag:tags) = do
        (rs', renderedTag) <- go rs tag
        (rs'', renderedTags) <- goList rs' tags
        Right (rs'', renderedTag <> renderedTags)

    go rs tag@(TT.TagBranch name as children) =
        case mathRequestFor 0 tag of
            Just _ -> case rs of
                response : rest -> Right (rest, TT.parseTree (responseHtml response))
                []              -> Left "KaTeX batch response was shorter than the request list"
            Nothing -> do
                (rest, children') <- goList rs children
                Right (rest, [TT.TagBranch name as children'])
    go rs tag = Right (rs, [tag])

validateResponses :: [KaTeXRequest] -> [KaTeXResponse] -> Either String [KaTeXResponse]
validateResponses requests responses = do
    let expected = map requestIndex requests
        actual = map responseIndex responses
    unless (expected == actual) $ Left $
        "KaTeX batch response indexes did not match request indexes: expected "
        <> show expected <> ", got " <> show actual
    pure responses

renderHtmlWithM :: Monad m
    => ([KaTeXRequest] -> m (Either String [KaTeXResponse]))
    -> String
    -> m (Either String String)
renderHtmlWithM renderRequests html = do
    let tree = TT.parseTree html
        requests = collectRequests tree
    responsesOrError <- if null requests
        then pure $ Right []
        else renderRequests requests
    pure $ do
        responses <- responsesOrError >>= validateResponses requests
        TT.renderTreeOptions tagSoupOption <$> renderTreeWith responses tree

renderHtmlWith :: ([KaTeXRequest] -> Either String [KaTeXResponse]) -> String -> Either String String
renderHtmlWith renderRequests = runIdentity . renderHtmlWithM (Identity . renderRequests)

renderKaTeXBatchWith :: Monad m
    => (BL.ByteString -> m BL.ByteString)
    -> [KaTeXRequest]
    -> m (Either String [KaTeXResponse])
renderKaTeXBatchWith runFilter requests =
    A.eitherDecode <$> runFilter (A.encode requests)

renderKaTeXBatch :: [KaTeXRequest] -> Compiler (Either String [KaTeXResponse])
renderKaTeXBatch =
    renderKaTeXBatchWith $ unixFilterLBS "tools/katex_runner.sh" ["--batch"]

renderHtmlCompiler :: String -> Compiler String
renderHtmlCompiler html = renderHtmlWithM renderKaTeXBatch html >>= either fail pure

render :: KaTeXRender
render = withItemBody renderHtmlCompiler
