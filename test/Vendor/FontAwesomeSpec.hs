{-# LANGUAGE OverloadedStrings #-}

module Vendor.FontAwesomeSpec (spec) where

import           Data.Aeson             (decode, encode, object, (.=))
import           Data.List              (sort)
import           Hakyll
import           System.FilePath        ((</>))
import           Test.Hspec
import           TestHelpers            (testCompile, withTestSite)
import qualified Text.HTML.TagSoup.Tree as TT
import           Vendor.FontAwesome     (renderWithSprite)

spec :: Spec
spec = describe "Font Awesome sprite rendering" $ do
    it "shares one symbol across icon sizes and preserves the icon attributes" $ do
        trees <- renderPage $ concat
            [ "<a aria-label=\"Instagram\"><i class=\"fab fa-instagram fa-lg\"></i></a>"
            , "<i class=\"fab fa-instagram fa-sm\"></i>"
            ]
        case trees of
            [ sprite@(TT.TagBranch "svg" _ [symbol@(TT.TagBranch "symbol" _ [path])])
              , TT.TagBranch "a" [("aria-label", "Instagram")] [large]
              , small
              ] -> do
                sprite `shouldHaveAttributes` [("style", "display:none"), ("aria-hidden", "true")]
                symbol `shouldHaveAttributes`
                    [("id", "fa-fab-instagram"), ("viewBox", "0 0 448 512")]
                case path of
                    TT.TagBranch "path" _ [] ->
                        path `shouldHaveAttributes` [("fill", "currentColor"), ("d", "M0 0")]
                    _ -> expectationFailure $ "Expected a path: " <> show path
                checkIcon "svg-inline--fa fa-instagram fa-lg" large
                checkIcon "svg-inline--fa fa-instagram fa-sm" small
            _ -> expectationFailure $ "Unexpected page body: " <> show trees

    it "leaves unrecognized icons and other SVGs alone and omits the empty sprite" $ do
        let html = "<i class=\"fas fa-unknown\"></i><svg role=\"img\"><title>Map</title></svg>"
        renderPage html `shouldReturn` TT.parseTree html
  where
    checkIcon cssClass icon@(TT.TagBranch "svg" _ children) = do
        icon `shouldHaveAttributes`
            [ ("class", cssClass), ("viewBox", "0 0 448 512"), ("aria-hidden", "true")
            , ("data-prefix", "fab"), ("data-icon", "instagram")
            ]
        children `shouldBe` [TT.TagBranch "use" [("href", "#fa-fab-instagram")] []]
    checkIcon _ tag = expectationFailure $ "Expected an SVG icon: " <> show tag

shouldHaveAttributes :: TT.TagTree String -> [(String, String)] -> Expectation
shouldHaveAttributes (TT.TagBranch _ attrs _) expected = sort attrs `shouldBe` sort expected
shouldHaveAttributes tag _ = expectationFailure $ "Expected an element: " <> show tag

renderPage :: String -> IO [TT.TagTree String]
renderPage html = withTestSite $ \_ cfg -> do
    icons <- maybe (fail "Could not decode the test icon") pure $ decode $ encode fixture
    testCompile cfg $ create [fromFilePath "icons.html"] $ do
        route idRoute
        compile $ makeItem ("<html><body>" <> html <> "</body></html>") >>= renderWithSprite icons
    result <- readFile $ destinationDirectory cfg </> "icons.html"
    case TT.parseTree result of
        [TT.TagBranch "html" _ [TT.TagBranch "body" _ children]] -> pure children
        _ -> fail $ "Unexpected page: " <> result
  where
    fixture = object ["fab" .= object ["instagram" .= object
        [ "tag" .= ("svg" :: String)
        , "attributes" .= object
            [ "class" .= ("svg-inline--fa fa-instagram" :: String)
            , "viewBox" .= ("0 0 448 512" :: String)
            , "aria-hidden" .= ("true" :: String)
            , "role" .= ("img" :: String)
            , "data-prefix" .= ("fab" :: String)
            , "data-icon" .= ("instagram" :: String)
            ]
        , "children" .= [object
            [ "tag" .= ("path" :: String)
            , "attributes" .= object ["fill" .= ("currentColor" :: String), "d" .= ("M0 0" :: String)]
            ]]
        ]]]
