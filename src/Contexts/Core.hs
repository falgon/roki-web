{-# LANGUAGE OverloadedStrings #-}
module Contexts.Core (
    siteMapDateCtx
  , blogTitleCtx
  , blogFontCtx
  , siteCtx
  , postCtx
  , listCtx
  , katexJsCtx
  , gSuiteCtx
) where

import           Data.List.Extra          (dropPrefix)
import           Data.String              (fromString)
import qualified Data.Text.Lazy           as TL
import           Hakyll
import           Lucid.Base               (Html, renderText)
import           Lucid.Html5
import           System.FilePath          (takeDirectory, (</>))

import           Config                   (GSuite (..), contentsRoot,
                                           defaultTimeLocale', gSuiteConf,
                                           siteName, timeZoneJST)
import           Config.Blog
import qualified Config.Blogs.AnotherBlog as BA
import qualified Config.Blogs.TechBlog    as TB
import           Contexts.Field           (descriptionField, imageField,
                                           localDateField, tagsField')

dateCtx :: Context String
dateCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y/%m/%d %R"

siteMapDateCtx :: Context String
siteMapDateCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y-%m-%d"

blogTitleCtx :: String -> Context String
blogTitleCtx = constField "blog-title"

blogFontCtx :: Html () -> Context String
blogFontCtx = constField "blog-font-html" . TL.unpack . renderText

techBlogCtx :: Context String
techBlogCtx = constField "tech-blog-title" TB.blogName
    <> constField "tech-blog-description" TB.blogDesc
    <> constField "tech-blog-issue-req" "https://github.com/falgon/roki-web/issues/new/choose"

privBlogCtx :: Context String
privBlogCtx = constField "diary-title" BA.blogName
    <> constField "diary-description" BA.blogDesc

blogCtx :: Context String
blogCtx = techBlogCtx <> privBlogCtx

authorCtx :: Context String
authorCtx = constField "author-name" "Roki"
    <> constField "author-avator" "/images/avator/prof1000x1000.png"
    <> constField "author-sex" "Male"
    <> constField "author-locale" "Tokyo, JP"
    <> constField "author-fav" fav
    <> constField "author-interested"
        "・FP&#10;・Compiler&#10;・Category theory&#10;・Low layer networking, Infrastructure"
    <> constField "author-job" "Software Engineer"
    <> constField "author-github" "falgon"
    <> constField "author-twitter" "roki_r7"
    <> constField "author-note" "_roki"
    <> constField "author-tumblr" "0x35"
    <> constField "author-reddit" "r0k1"
    <> constField "author-stackoverflow" "8345717"
    <> constField "author-steam" "r0k1"
    <> constField "author-yukicoder" "3223"
    <> constField "author-teratail" "kjfkhfhgx"
    <> constField "google-analytics" "UA-116653080-2"
    where
        fav = TL.unpack $ renderText $
            ul_ [style_ "margin: 0;", class_ "comma-list"] $ do
                li_ "Coffee"
                li_ "Watches"
                li_ $ a_
                    [href_ "https://www.san-x.co.jp/rilakkuma/profile/#&gid=1&pid=3"]
                    "Kiiroitori"

siteCtx :: Context String
siteCtx = constField "lang" "ja"
    <> constField "site-title" siteName
    <> constField "site-description" "This is a Roki's website."
    <> constField "copyright" "copyright &copy; 2016~ Roki All Rights Reserved."
    <> blogCtx
    <> authorCtx

postCtx :: Bool -> Tags -> Context String
postCtx isPreview tags = dateCtx
    <> tagsField' "tags" tags
    <> descriptionField "description" 150
    <> imageField "image"
    <> siteCtx
    <> jsPathCtx
    <> defaultContext
    <> if isPreview then katexJsCtx else mempty

listCtx :: Bool -> Context String
listCtx isPreview = siteCtx
    <> bodyField "body"
    <> metadataField
    <> pathField "path"
    <> urlField "url"
    <> if isPreview then katexJsCtx else mempty

katexJsCtx :: Context String
katexJsCtx = constField "katex-script" $ TL.unpack $ renderText $ do
    script_ [defer_ "", type_ "text/javascript", src_ "/vendor/katex/katex.min.js"] TL.empty
    script_ [defer_ "", type_ "text/javascript", src_ "/vendor/katex/auto-render.min.js"] TL.empty

jsPathCtx :: Context String
jsPathCtx = listFieldWith "js" ctx $ \item -> do
    mds <- getMetadataField (itemIdentifier item) "js"
    return $ case mds of
        Just xs -> map (itemize item . trim) $ splitAll "," xs
        Nothing -> []
    where
        ctx = field "src-script" (return . itemBody)
        itemize item md = Item {
            itemIdentifier = fromString md
          , itemBody = jsDirPath item </> md
        }
        jsDirPath s = dropPrefix contentsRoot $ takeDirectory $
            toFilePath (itemIdentifier s)

gSuiteCtx :: BlogConfig m -> Context String
gSuiteCtx bc = constField "google-cx" (gCxPrefix gSuiteConf <> ":" <> blogGoogleCx bc)
    <> constField "google-site-verification" (gSiteVerifyKey gSuiteConf)
