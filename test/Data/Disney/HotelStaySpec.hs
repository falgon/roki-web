module Data.Disney.HotelStaySpec (spec) where

import           Control.Monad         (unless)
import           Data.Disney.HotelStay
import           Data.List             (sortOn)
import           System.Directory      (doesFileExist)
import           System.FilePath       ((</>))
import           System.IO             (IOMode (WriteMode), hClose, hPutChar,
                                        hPutStr, hSetBinaryMode, hSetEncoding,
                                        utf8, withFile)
import           System.IO.Temp        (withSystemTempDirectory,
                                        withSystemTempFile)
import           Test.Hspec

spec :: Spec
spec = do
    describe "deriveHotels" $ do
        it "derives a hotel's stay count from its structured breakdown" $ do
            let rawHotel = HotelRaw
                    { hotelCodeRaw = "FSH"
                    , detailsRaw =
                        [ HotelStayRaw ["ファンタジーシャトー", "ローズコートサイド", "スーペリア"] 3
                        , HotelStayRaw ["ファンタジーシャトー", "ローズコートサイド", "スーペリア・アルコーヴ"] 2
                        , HotelStayRaw ["ファンタジーシャトー", "べイエリアサイド", "スーペリア・アルコーヴ"] 1
                        , HotelStayRaw ["ファンタジーシャトー", "スプリングスサイド", "バルアル"] 1
                        ]
                    , hotelColorRaw = "#854454"
                    }

            hotels <- expectRight $ deriveHotels [rawHotel]
            map hotelStayCount hotels `shouldBe` [7]

        it "preserves the configured hierarchy and insertion order" $ do
            let rawHotel = HotelRaw
                    { hotelCodeRaw = "DHM"
                    , detailsRaw =
                        [ HotelStayRaw ["スイート", "ハバグラ"] 1
                        , HotelStayRaw ["スイート", "ハバテラ"] 2
                        , HotelStayRaw ["ポルトパラディーゾ", "スーペリアルームハーバービュー"] 1
                        ]
                    , hotelColorRaw = "#8A7501"
                    }

            hotels <- expectRight $ deriveHotels [rawHotel]
            map details hotels `shouldBe`
                [ [ HDNode "スイート"
                        [ HDStay "ハバグラ" 1
                        , HDStay "ハバテラ" 2
                        ]
                  , HDNode "ポルトパラディーゾ"
                        [HDStay "スーペリアルームハーバービュー" 1]
                  ]
                ]

        it "keeps a branch at its first position when later paths revisit it" $ do
            let rawHotel = hotelWith
                    [ HotelStayRaw ["B", "first"] 1
                    , HotelStayRaw ["A", "only"] 1
                    , HotelStayRaw ["B", "second"] 1
                    ]

            hotels <- expectRight $ deriveHotels [rawHotel]
            map details hotels `shouldBe`
                [ [ HDNode "B" [HDStay "first" 1, HDStay "second" 1]
                  , HDNode "A" [HDStay "only" 1]
                  ]
                ]

        it "rejects invalid and ambiguous breakdowns" $ do
            deriveHotels [hotelWith []]
                `shouldBe` Left (EmptyHotelDetails "TDH")
            deriveHotels [hotelWith [HotelStayRaw [] 1]]
                `shouldBe` Left (EmptyDetailPath "TDH")
            deriveHotels [hotelWith [HotelStayRaw [" "] 1]]
                `shouldBe` Left (EmptyDetailLabel "TDH" [" "])
            deriveHotels [hotelWith [HotelStayRaw ["スーペリア"] 0]]
                `shouldBe` Left (ZeroStayCount "TDH" ["スーペリア"])
            deriveHotels [hotelWith $ replicate 2 (HotelStayRaw ["スーペリア"] 1)]
                `shouldBe` Left (DuplicateDetailPath "TDH" ["スーペリア"])
            deriveHotels [hotelWith
                [ HotelStayRaw ["スーペリア"] 1
                , HotelStayRaw ["スーペリア", "コーナールーム"] 1
                ]]
                `shouldBe` Left
                    (ConflictingDetailPath "TDH" ["スーペリア"] ["スーペリア", "コーナールーム"])

        it "rejects empty and duplicate hotel codes" $ do
            deriveHotels [] `shouldBe` Left EmptyHotels
            deriveHotels [hotelWithCode ""] `shouldBe` Left (EmptyHotelCode 1)
            deriveHotels [hotelWithCode "TDH", hotelWithCode "   "]
                `shouldBe` Left (EmptyHotelCode 2)
            deriveHotels [hotelWithCode "TDH\"><script>"]
                `shouldBe` Left (InvalidHotelCode "TDH\"><script>")
            deriveHotels [hotelWithCode "tdh"]
                `shouldBe` Left (InvalidHotelCode "tdh")
            deriveHotels [hotelWithCode "TDH", hotelWithCode "TDH"]
                `shouldBe` Left (DuplicateHotelCode "TDH")

        it "rejects colors that are unsafe for the generated style attribute" $ do
            hotel <- expectSingleHotel $ deriveHotels [hotelWithColor "#abcdef"]
            hotelColor hotel `shouldBe` "#abcdef"
            mapM_ (\color ->
                deriveHotels [hotelWithColor color]
                    `shouldBe` Left (InvalidHotelColor "TDH" color))
                [ "#FFF"
                , "#1234567"
                , "#GGGGGG"
                , "B95C00"
                , "red; background-image: url(javascript:alert(1))"
                ]

    describe "formatStayLabel" $ do
        it "keeps single stays unchanged and appends the existing suffix for multiple stays" $ do
            formatStayLabel "ハバグラ" 1 `shouldBe` "ハバグラ"
            formatStayLabel "スーペリア" 3 `shouldBe` "スーペリア \x00D7\&3"

    describe "describeHotelConfigError" $ do
        it "includes the source path and readable detail path" $ do
            describeHotelConfigError productionHotelsPath
                (ZeroStayCount "TDH" ["スーペリア", "コーナールーム"])
                `shouldBe`
                    "Invalid hotel configuration in "
                        ++ productionHotelsPath
                        ++ ": hotel TDH has zero stays at path: スーペリア > コーナールーム"

        it "identifies an empty hotel code by its one-based index" $ do
            describeHotelConfigError "Hotels.dhall" (EmptyHotelCode 2)
                `shouldBe`
                    "Invalid hotel configuration in Hotels.dhall: hotel at index 2 has an empty code"

        it "uses the readable configuration error for uncaught exception output" $ do
            let configError = ZeroStayCount "TDH" ["スーペリア"]
                exception = HotelConfigException "Hotels.dhall" configError
            show exception `shouldBe` describeHotelConfigError "Hotels.dhall" configError

    describe "renderHotelDetailsHtml" $ do
        it "escapes detail labels before inserting them into generated HTML" $ do
            renderHotelDetailsHtml [HDStay "</span><script>alert(1)</script>" 1]
                `shouldBe`
                    "<span class=\"hotel-detail-item hotel-detail-level-0\">&lt;/span&gt;&lt;script&gt;alert(1)&lt;/script&gt;</span>"

            renderHotelDetailsHtml
                [HDNode "</span><script>alert(2)</script>" [HDStay "safe" 1]]
                `shouldBe` concat
                    [ "<span class=\"hotel-detail-item hotel-detail-level-0\">&lt;/span&gt;&lt;script&gt;alert(2)&lt;/script&gt;</span>"
                    , "<span class=\"hotel-detail-item hotel-detail-level-1\">safe</span>"
                    ]

        it "clamps detail nesting at the existing level-3 CSS class" $ do
            let nestedDetails =
                    [HDNode "0" [HDNode "1" [HDNode "2" [HDNode "3" [HDStay "4" 1]]]]]
            renderHotelDetailsHtml nestedDetails `shouldBe` concat
                [ detailSpan 0 "0"
                , detailSpan 1 "1"
                , detailSpan 2 "2"
                , detailSpan 3 "3"
                , detailSpan 3 "4"
                ]

    describe "Hotels.dhall" $ do
        it "reads UTF-8 and resolves relative imports from the config directory" $
            withSystemTempDirectory "hotel-config" $ \directory -> do
                let configPath = directory </> "Hotels.dhall"
                    importedPath = directory </> "Hotel.dhall"
                writeUtf8File configPath "[ ./Hotel.dhall ]"
                writeUtf8File importedPath relativeHotelConfig

                hotels <- loadHotels configPath
                map (\hotel -> (hotelCode hotel, hotelStayCount hotel)) hotels
                    `shouldBe` [("TDH", 2)]

        -- These production-data assertions are an intentional migration baseline.
        -- Update them together whenever the authored hotel breakdown changes.
        it "loads a regular FilePath and preserves current stay totals" $ do
            assertProductionHotelConfigExists
            hotels <- loadHotels productionHotelsPath
            map (\hotel -> (hotelCode hotel, hotelStayCount hotel)) (sortOn hotelCode hotels)
                `shouldBe` [("DHM", 5), ("FSH", 7), ("TDH", 6), ("TSH", 3)]
            sum (map hotelStayCount hotels) `shouldBe` 21

        it "renders the production hotel details exactly as before the migration" $ do
            assertProductionHotelConfigExists
            hotels <- loadHotels productionHotelsPath
            map (\hotel -> (hotelCode hotel, renderHotelDetailsHtml $ details hotel)) hotels
                `shouldBe` expectedHotelDetailsHtml

        it "reports validation failures with the source path and readable reason" $
            withSystemTempFile "Hotels-invalid.dhall" $ \path handle -> do
                hSetEncoding handle utf8
                hPutStr handle invalidHotelConfig
                hClose handle
                loadHotels path `shouldThrow` readableHotelConfigError path

        it "reports invalid UTF-8 with the source path and readable reason" $
            withSystemTempFile "Hotels-invalid-utf8.dhall" $ \path handle -> do
                hSetBinaryMode handle True
                hPutChar handle '\xFF'
                hClose handle
                loadHotels path `shouldThrow` invalidUtf8HotelConfigError path
  where
    hotelWith rawDetails = HotelRaw "TDH" rawDetails "#B95C00"
    hotelWithCode code = hotelWithCodeAndDetails code [HotelStayRaw ["スーペリア"] 1]
    hotelWithCodeAndDetails code rawDetails = HotelRaw code rawDetails "#B95C00"
    hotelWithColor color = HotelRaw "TDH" [HotelStayRaw ["スーペリア"] 1] color

assertProductionHotelConfigExists :: Expectation
assertProductionHotelConfigExists = do
    exists <- doesFileExist productionHotelsPath
    unless exists $ expectationFailure $
        "Production hotel config not found from the test working directory: "
            ++ productionHotelsPath

detailSpan :: Int -> String -> String
detailSpan level text =
    "<span class=\"hotel-detail-item hotel-detail-level-"
        ++ show level
        ++ "\">"
        ++ text
        ++ "</span>"

expectedHotelDetailsHtml :: [(String, String)]
-- Update this migration golden when the production hotel breakdown intentionally changes.
expectedHotelDetailsHtml =
    [ ( "FSH"
      , concat
          [ "<span class=\"hotel-detail-item hotel-detail-level-0\">ファンタジーシャトー</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">スプリングスサイド</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-2\">バルアル</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">ローズコートサイド</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-2\">スーペリア \x00D7\&3</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-2\">スーペリア・アルコーヴ \x00D7\&2</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">べイエリアサイド</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-2\">スーペリア・アルコーヴ</span>"
          ]
      )
    , ( "DHM"
      , concat
          [ "<span class=\"hotel-detail-item hotel-detail-level-0\">スイート</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">ハバグラ</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">ハバテラ \x00D7\&2</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">ピアバル</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-0\">ポルトパラディーゾ</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">スーペリアルームハーバービュー</span>"
          ]
      )
    , ( "TDH"
      , concat
          [ "<span class=\"hotel-detail-item hotel-detail-level-0\">キャラ</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">美女野獣</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">シンデレラ</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-0\">スーペリア</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">コーナールーム \x00D7\&2</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">パークグランドビュー</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-0\">コンシェルジュ</span>"
          , "<span class=\"hotel-detail-item hotel-detail-level-1\">バルコニールーム パークグランドビュー</span>"
          ]
      )
    , ( "TSH"
      , "<span class=\"hotel-detail-item hotel-detail-level-0\">スタンダードルーム \x00D7\&3</span>"
      )
    ]

invalidHotelConfig :: String
invalidHotelConfig =
    "[ { hotelCodeRaw = \"TDH\", detailsRaw = [ { detailPathRaw = [ \"スーペリア\" ], stayCountRaw = 0 } ], hotelColorRaw = \"#B95C00\" } ]"

relativeHotelConfig :: String
relativeHotelConfig =
    "{ hotelCodeRaw = \"TDH\", detailsRaw = [ { detailPathRaw = [ \"スーペリア\" ], stayCountRaw = 2 } ], hotelColorRaw = \"#B95C00\" }"

writeUtf8File :: FilePath -> String -> IO ()
writeUtf8File path contents =
    withFile path WriteMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle contents

readableHotelConfigError :: FilePath -> HotelConfigException -> Bool
readableHotelConfigError path (HotelConfigException actualPath configError) =
    actualPath == path
        && configError == ZeroStayCount "TDH" ["スーペリア"]

invalidUtf8HotelConfigError :: FilePath -> HotelConfigException -> Bool
invalidUtf8HotelConfigError path (HotelConfigException actualPath configError) =
    actualPath == path
        && case configError of
            InvalidUtf8 reason -> not $ null reason
            _                  -> False

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
    Left err -> do
        expectationFailure $ "Expected Right, got Left: " ++ show err
        fail "unreachable"
    Right value -> pure value

expectSingleHotel :: Show error => Either error [Hotel] -> IO Hotel
expectSingleHotel result = do
    hotels <- expectRight result
    case hotels of
        [hotel] -> pure hotel
        _ -> do
            expectationFailure $ "Expected one hotel, got: " ++ show hotels
            fail "unreachable"
