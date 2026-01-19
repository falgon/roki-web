{-# LANGUAGE OverloadedStrings #-}
module Contexts.Core (
    dateCtx
  , jsPathCtx
  , siteMapDateCtx
  , canonicalUrlField
  , siteCtx
) where

import           Control.Applicative      (empty)
import           Control.Monad            ((<=<))
import           Data.Functor             ((<&>))
import           Data.List.Extra          (dropPrefix, mconcatMap)
import           Data.String              (fromString)
import qualified Data.Text.Lazy           as TL
import           Hakyll
import           Lucid.Base               (renderText)
import           Lucid.Html5
import           System.FilePath          (takeDirectory, (</>))

import           Config                   (baseUrl, contentsRoot,
                                           defaultTimeLocale', siteName,
                                           timeZoneJST)
import qualified Config.Blogs.AnotherBlog as BA
import qualified Config.Blogs.TechBlog    as TB
import           Contexts.Field           (jsonLdPersonField,
                                           jsonLdWebSiteField, localDateField)

dateCtx :: Context String
dateCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y/%m/%d %R"

siteMapDateCtx :: Context String
siteMapDateCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y-%m-%d"

canonicalUrlField :: Context a
canonicalUrlField = field "canonical-url" $
    maybe empty (return . ((baseUrl <>) . toUrl)) <=< getRoute . itemIdentifier

techBlogCtx :: Context String
techBlogCtx = mconcatMap (uncurry constField) [
    ("tech-blog-title", TB.blogName)
  , ("tech-blog-description", TB.blogDesc)
  , ("tech-blog-issue-req", "https://github.com/falgon/roki-web/issues/new/choose")
  ]

privBlogCtx :: Context String
privBlogCtx = mconcatMap (uncurry constField) [
    ("diary-title", BA.blogName)
  , ("diary-description", BA.blogDesc)
  ]

blogCtx :: Context String
blogCtx = techBlogCtx <> privBlogCtx

authorCtx :: Context String
authorCtx = mconcatMap (uncurry constField) [
    ("author-name", "Roki")
  , ("author-avator", "/images/avator/prof1000x1000.png")
  , ("author-sex", "Male")
  , ("author-locale", "Tokyo, JP")
  , ("author-fav", fav)
  , ("author-interested", "・FP&#10;・Compiler&#10;・Category theory&#10;・Low layer networking, Infrastructure")
  , ("author-job", "Software Engineer")
  , ("author-github", "falgon")
  , ("author-twitter", "roki_r7")
  , ("author-note", "_roki")
  , ("author-tumblr", "0x35")
  , ("author-reddit", "r0k1")
  , ("author-stackoverflow", "8345717")
  , ("author-steam", "r0k1")
  , ("author-yukicoder", "3223")
  , ("author-teratail", "kjfkhfhgx")
  , ("google-analytics", "G-D7B9XXQWJE")
  ]
    where
        fav = TL.unpack $ renderText $
            ul_ [style_ "margin: 0;", class_ "comma-list"] $ do
                li_ "Coffee"
                li_ "Watches"
                li_ $ a_
                    [href_ "https://www.san-x.co.jp/rilakkuma/profile/#&gid=1&pid=3"]
                    "Kiiroitori"
                li_ $ a_
                    [href_ "disney_experience_summary/jp.html"]
                    "Disney"

siteCtx :: Context String
siteCtx = mconcat [
    canonicalUrlField
  , constField "lang" "ja"
  , constField "site-title" siteName
  , constField "site-description" "This is a Roki's website."
  , constField "copyright" "copyright &copy; 2016~ Roki All Rights Reserved."
  , blogCtx
  , authorCtx
  , jsonLdPersonField "json-ld-person"
  , jsonLdWebSiteField "json-ld-website"
  ]

jsPathCtx :: Context String
jsPathCtx = listFieldWith "js" ctx $ \item ->
    getMetadataField (itemIdentifier item) "js" <&>
        maybe mempty (map (itemize item . trim) . splitAll ",")
    where
        ctx = field "src-script" (return . itemBody)
        itemize item md = Item {
            itemIdentifier = fromString md
          , itemBody = jsDirPath item </> md
          }
        jsDirPath = dropPrefix contentsRoot
            . takeDirectory
            . toFilePath
            . itemIdentifier
