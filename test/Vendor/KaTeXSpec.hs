module Vendor.KaTeXSpec (spec) where

import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as BL
import           Data.Functor.Identity (Identity (..), runIdentity)
import           Data.List             (isPrefixOf)
import           Test.Hspec
import qualified Text.HTML.TagSoup     as TS
import           Vendor.KaTeX

spec :: Spec
spec = do
    describe "renderHtmlWith" $ do
        it "does not invoke the renderer when the page has no math" $
            renderHtmlWith (const $ Left "renderer must not be called") "<p>no math</p>"
                `shouldBe` Right "<p>no math</p>"

        it "does not treat math-like classes as math" $
            renderHtmlWith (const $ Left "renderer must not be called") "<span class=\"not-math\">x</span>"
                `shouldBe` Right "<span class=\"not-math\">x</span>"

        it "preserves surrounding and nested HTML when replacing math nodes" $ do
            let input = "<section><p>a<span class=\"math inline\">x</span><em>b</em></p></section>"
                renderRequests [request] = Right
                    [KaTeXResponse (requestIndex request) "<strong>x</strong>"]
                renderRequests _ = Left "unexpected request count"
            renderHtmlWith renderRequests input
                `shouldBe` Right "<section><p>a<strong>x</strong><em>b</em></p></section>"

        it "replaces mixed inline and display math in request order" $ do
            let input = concat
                    [ "<p>"
                    , "<span class=\"math inline\">x+y</span>"
                    , "<span class=\"math display\">z</span>"
                    , "</p>"
                    ]
                renderRequests requests = Right
                    [ KaTeXResponse (requestIndex request)
                        $ "<span data-index=\"" <> show (requestIndex request) <> "\">"
                        <> requestMath request
                        <> ":"
                        <> show (requestDisplayMode request)
                        <> "</span>"
                    | request <- requests
                    ]
            renderHtmlWith renderRequests input `shouldBe` Right
                "<p><span data-index=\"0\">x+y:False</span><span data-index=\"1\">z:True</span></p>"

        it "preserves UTF-8 math text in batch requests" $ do
            let input = "<span class=\"math inline\">日本語+α</span>"
                renderRequests [request] = Right
                    [KaTeXResponse (requestIndex request) $ "<b>" <> requestMath request <> "</b>"]
                renderRequests _ = Left "unexpected request count"
            renderHtmlWith renderRequests input `shouldBe` Right "<b>日本語+α</b>"

        it "round-trips JSON wire bytes with Unicode and special characters" $ do
            let math = "\\text{日本語 <>& \" \\\\}"
                input = "<span class=\"math inline\">" <> TS.escapeHTML math <> "</span>"
                fakeWire :: BL.ByteString -> Identity BL.ByteString
                fakeWire bytes = Identity $ case A.eitherDecode bytes of
                    Right [request]
                        | requestIndex request == 0
                        , requestMath request == math
                        , not (requestDisplayMode request) -> A.encode
                            [KaTeXResponse 0 "<span data-quote=\"&quot;\">日本語 &amp; &lt;&gt; \\</span>"]
                    _ -> A.encode [KaTeXResponse 99 "bad request"]
            runIdentity (renderHtmlWithM (renderKaTeXBatchWith fakeWire) input)
                `shouldBe` Right "<span data-quote=\"&quot;\">日本語 &amp; &lt;&gt; \\</span>"

        it "rejects malformed batch responses" $ do
            let input = "<span class=\"math inline\">x</span>"
                renderRequests _ = Right [KaTeXResponse 1 "<b>x</b>"]
            renderHtmlWith renderRequests input `shouldSatisfy` either
                (("KaTeX batch response indexes did not match request indexes" `isPrefixOf`))
                (const False)

        it "rejects missing batch responses" $ do
            let input = "<span class=\"math inline\">x</span>"
                renderRequests _ = Right []
            renderHtmlWith renderRequests input `shouldSatisfy` either
                (("KaTeX batch response indexes did not match request indexes" `isPrefixOf`))
                (const False)

        it "rejects extra batch responses" $ do
            let input = "<span class=\"math inline\">x</span>"
                renderRequests _ = Right [KaTeXResponse 0 "<b>x</b>", KaTeXResponse 1 "<b>y</b>"]
            renderHtmlWith renderRequests input `shouldSatisfy` either
                (("KaTeX batch response indexes did not match request indexes" `isPrefixOf`))
                (const False)

        it "rejects reordered batch responses" $ do
            let input = "<span class=\"math inline\">x</span><span class=\"math inline\">y</span>"
                renderRequests _ = Right [KaTeXResponse 1 "<b>y</b>", KaTeXResponse 0 "<b>x</b>"]
            renderHtmlWith renderRequests input `shouldSatisfy` either
                (("KaTeX batch response indexes did not match request indexes" `isPrefixOf`))
                (const False)

        it "rejects duplicate batch response indexes" $ do
            let input = "<span class=\"math inline\">x</span><span class=\"math inline\">y</span>"
                renderRequests _ = Right [KaTeXResponse 0 "<b>x</b>", KaTeXResponse 0 "<b>again</b>"]
            renderHtmlWith renderRequests input `shouldSatisfy` either
                (("KaTeX batch response indexes did not match request indexes" `isPrefixOf`))
                (const False)

        it "propagates renderer errors" $
            renderHtmlWith (const $ Left "bad formula") "<span class=\"math inline\">\\bad</span>"
                `shouldBe` Left "bad formula"
