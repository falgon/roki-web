{-# LANGUAGE DeriveGeneric #-}

module Data.Disney.HotelStay (
    Hotel,
    HotelConfigException (..),
    HotelConfigError (..),
    HotelDetail (..),
    HotelRaw (..),
    HotelStayRaw (..),
    describeHotelConfigError,
    details,
    deriveHotels,
    formatStayLabel,
    hotelCode,
    hotelColor,
    hotelStayCount,
    loadHotels,
    productionHotelsPath,
    renderHotelDetailsHtml,
) where

import           Control.Exception     (Exception (displayException), throwIO)
import qualified Data.ByteString       as BS
import           Data.Char             (isHexDigit, isSpace)
import           Data.Foldable         (traverse_)
import           Data.Functor.Identity (Identity (..))
import           Data.List             (findIndex, foldl', intercalate,
                                        isPrefixOf)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.Lazy        as TL
import           Dhall                 (FromDhall, Generic, Natural, auto,
                                        defaultInputSettings, inputWithSettings,
                                        rootDirectory, sourceName)
import qualified Lucid
import           System.FilePath       (takeDirectory)

data HotelDetail
  = HDStay String Natural
  | HDNode String [HotelDetail]
  deriving (Eq, Show)

data Hotel = Hotel String [HotelDetail] String
  deriving (Eq, Show)

hotelCode :: Hotel -> String
hotelCode (Hotel code _ _) = code

details :: Hotel -> [HotelDetail]
details (Hotel _ hotelDetails _) = hotelDetails

hotelColor :: Hotel -> String
hotelColor (Hotel _ _ color) = color

data HotelStayRaw = HotelStayRaw {
    detailPathRaw :: [String]
  , stayCountRaw  :: Natural
  } deriving (Eq, Generic, Show)

instance FromDhall HotelStayRaw

data HotelRaw = HotelRaw {
    hotelCodeRaw  :: String
  , detailsRaw    :: [HotelStayRaw]
  , hotelColorRaw :: String
  } deriving (Eq, Generic, Show)

instance FromDhall HotelRaw

data HotelConfigError
  = EmptyHotels
  | EmptyHotelCode Int
  | InvalidUtf8 String
  | InvalidHotelCode String
  | DuplicateHotelCode String
  | InvalidHotelColor String String
  | EmptyHotelDetails String
  | EmptyDetailPath String
  | EmptyDetailLabel String [String]
  | ZeroStayCount String [String]
  | DuplicateDetailPath String [String]
  | ConflictingDetailPath String [String] [String]
  deriving (Eq, Show)

data HotelConfigException = HotelConfigException FilePath HotelConfigError
  deriving (Eq)

instance Show HotelConfigException where
    show (HotelConfigException path configError) =
        describeHotelConfigError path configError

instance Exception HotelConfigException where
    displayException (HotelConfigException path configError) =
        describeHotelConfigError path configError

productionHotelsPath :: FilePath
productionHotelsPath = "contents/config/disney/Hotels.dhall"

loadHotels :: FilePath -> IO [Hotel]
loadHotels path = do
    bytes <- BS.readFile path
    source <- either (throwIO . HotelConfigException path . InvalidUtf8 . show) pure $
        TE.decodeUtf8' bytes
    rawHotels <- inputWithSettings settings auto source
    either (throwIO . HotelConfigException path) pure $
        deriveHotels rawHotels
  where
    settings =
        set sourceName path $
            set rootDirectory (takeDirectory path) defaultInputSettings

    set lens value = runIdentity . lens (const $ Identity value)

deriveHotels :: [HotelRaw] -> Either HotelConfigError [Hotel]
deriveHotels [] = Left EmptyHotels
deriveHotels rawHotels = do
    validateHotelCodes $ map hotelCodeRaw rawHotels
    traverse deriveHotel rawHotels

validateHotelCodes :: [String] -> Either HotelConfigError ()
validateHotelCodes codes
    | Just index <- findIndex isBlank codes = Left $ EmptyHotelCode (index + 1)
    | Just invalidCode <- firstInvalidHotelCode codes = Left $ InvalidHotelCode invalidCode
    | otherwise = case firstDuplicate codes of
        Just code -> Left $ DuplicateHotelCode code
        Nothing   -> Right ()

deriveHotel :: HotelRaw -> Either HotelConfigError Hotel
deriveHotel rawHotel = do
    validateHotelColor code $ hotelColorRaw rawHotel
    validateStayDetails code rawDetails
    pure $ Hotel code (buildHotelDetails rawDetails) (hotelColorRaw rawHotel)
  where
    code = hotelCodeRaw rawHotel
    rawDetails = detailsRaw rawHotel

validateStayDetails :: String -> [HotelStayRaw] -> Either HotelConfigError ()
validateStayDetails code rawDetails
    | null rawDetails = Left $ EmptyHotelDetails code
    | otherwise = do
        traverse_ validateDetail rawDetails
        case firstDuplicate paths of
            Just duplicatePath -> Left $ DuplicateDetailPath code duplicatePath
            Nothing -> case firstPathConflict paths of
                Just (shorterPath, longerPath) ->
                    Left $ ConflictingDetailPath code shorterPath longerPath
                Nothing -> Right ()
  where
    paths = map detailPathRaw rawDetails
    validateDetail rawDetail
        | null path = Left $ EmptyDetailPath code
        | any isBlank path = Left $ EmptyDetailLabel code path
        | stayCountRaw rawDetail == 0 = Left $ ZeroStayCount code path
        | otherwise = Right ()
      where
        path = detailPathRaw rawDetail

buildHotelDetails :: [HotelStayRaw] -> [HotelDetail]
buildHotelDetails = foldl' (flip insertStayPath) []

insertStayPath :: HotelStayRaw -> [HotelDetail] -> [HotelDetail]
-- Empty paths are rejected by validateStayDetails before this internal builder runs.
insertStayPath (HotelStayRaw [] _) forest = forest
insertStayPath (HotelStayRaw [leaf] count) forest = insertStay leaf count forest
insertStayPath (HotelStayRaw (label:rest) count) forest =
    insertBranch label (HotelStayRaw rest count) forest

insertStay :: String -> Natural -> [HotelDetail] -> [HotelDetail]
insertStay label count []            = [HDStay label count]
insertStay label count (detail:rest) = detail : insertStay label count rest

insertBranch :: String -> HotelStayRaw -> [HotelDetail] -> [HotelDetail]
insertBranch label rawDetail [] =
    [HDNode label (insertStayPath rawDetail [])]
insertBranch label rawDetail (HDNode nodeLabel children:rest)
    | nodeLabel == label = HDNode label (insertStayPath rawDetail children) : rest
insertBranch label rawDetail (detail:rest) =
    detail : insertBranch label rawDetail rest

hotelStayCount :: Hotel -> Natural
hotelStayCount = sum . map detailStayCount . details
  where
    detailStayCount (HDStay _ count)    = count
    detailStayCount (HDNode _ children) = sum $ map detailStayCount children

formatStayLabel :: String -> Natural -> String
formatStayLabel label 1     = label
formatStayLabel label count = label ++ " ×" ++ show count

describeHotelConfigError :: FilePath -> HotelConfigError -> String
describeHotelConfigError path configError =
    "Invalid hotel configuration in " ++ path ++ ": " ++ describe configError
  where
    describe EmptyHotels = "at least one hotel is required"
    describe (EmptyHotelCode index) =
        "hotel at index " ++ show index ++ " has an empty code"
    describe (InvalidUtf8 reason) = "file is not valid UTF-8: " ++ reason
    describe (InvalidHotelCode code) =
        "hotel code must contain only A-Z, 0-9, '_' or '-': " ++ code
    describe (DuplicateHotelCode code) = "duplicate hotel code: " ++ code
    describe (InvalidHotelColor code color) =
        "hotel " ++ code ++ " has an invalid #RRGGBB color: " ++ color
    describe (EmptyHotelDetails code) = "hotel " ++ code ++ " has no stay details"
    describe (EmptyDetailPath code) = "hotel " ++ code ++ " has an empty detail path"
    describe (EmptyDetailLabel code detailPath) =
        "hotel " ++ code ++ " has an empty label in path: " ++ displayPath detailPath
    describe (ZeroStayCount code detailPath) =
        "hotel " ++ code ++ " has zero stays at path: " ++ displayPath detailPath
    describe (DuplicateDetailPath code detailPath) =
        "hotel " ++ code ++ " has a duplicate path: " ++ displayPath detailPath
    describe (ConflictingDetailPath code shorterPath longerPath) =
        "hotel " ++ code ++ " has conflicting paths: "
            ++ displayPath shorterPath
            ++ " and "
            ++ displayPath longerPath

    displayPath = intercalate " > "

renderHotelDetailsHtml :: [HotelDetail] -> String
renderHotelDetailsHtml = concatMap (renderDetail 0)
  where
    renderDetail level (HDStay label count) =
        renderSpan level $ formatStayLabel label count
    renderDetail level (HDNode label children) =
        renderSpan level label ++ concatMap (renderDetail (level + 1)) children

    renderSpan level text =
        let classLevel = min level 3
            className = "hotel-detail-item hotel-detail-level-" ++ show classLevel
        in TL.unpack $ Lucid.renderText $
            Lucid.span_ [Lucid.class_ $ T.pack className] (Lucid.toHtml $ T.pack text)

firstInvalidHotelCode :: [String] -> Maybe String
firstInvalidHotelCode = firstMatch . filter (not . validHotelCode)
  where
    validHotelCode = all isAllowedCodeCharacter
    isAllowedCodeCharacter character =
        ('A' <= character && character <= 'Z')
            || ('0' <= character && character <= '9')
            || character == '_'
            || character == '-'

validateHotelColor :: String -> String -> Either HotelConfigError ()
validateHotelColor code color = case color of
    '#':hexDigits
        | length hexDigits == 6 && all isHexDigit hexDigits -> Right ()
    _ -> Left $ InvalidHotelColor code color

firstDuplicate :: Eq a => [a] -> Maybe a
firstDuplicate values = findDuplicate values []
  where
    findDuplicate [] _ = Nothing
    findDuplicate (value:rest) seen
        | value `elem` seen = Just value
        | otherwise = findDuplicate rest (value : seen)

firstPathConflict :: Eq a => [[a]] -> Maybe ([a], [a])
firstPathConflict paths = firstMatch
    [ (shorterPath, longerPath)
    | shorterPath <- paths
    , longerPath <- paths
    , shorterPath /= longerPath
    , shorterPath `isPrefixOf` longerPath
    ]

firstMatch :: [a] -> Maybe a
firstMatch []      = Nothing
firstMatch (x : _) = Just x

isBlank :: String -> Bool
isBlank = all isSpace
