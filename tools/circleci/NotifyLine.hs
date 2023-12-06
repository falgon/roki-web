#!/usr/bin/env stack
-- stack script --resolver lts-14.27 --package bytestring --package case-insensitive --package http-conduit --package utf8-string --package uri-encode --package optparse-applicative

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.UTF8            as BU
import qualified Data.CaseInsensitive            as CI
import           Data.Functor                    ((<&>))
import           Network.HTTP.Conduit            (RequestBody (..),
                                                  requestHeaders)
import           Network.HTTP.Simple
import           Network.URI.Encode              (encodeTextToBS)
import qualified Options.Applicative             as OA
import qualified Options.Applicative.Help.Pretty as OA
import           System.Environment              (getEnv)

buildBearerAuth :: B.ByteString -> B.ByteString
buildBearerAuth = B.append "Bearer "

applyBearerAuth :: B.ByteString -> Request -> Request
applyBearerAuth bearerToken req = setRequestHeaders (authHeader : requestHeaders req) req
    where
        authHeader = (CI.mk "Authorization", buildBearerAuth bearerToken)

setRequestBearerAuth :: B.ByteString -> Request -> Request
setRequestBearerAuth = applyBearerAuth

lineNotifyAPIEndPoint :: String
lineNotifyAPIEndPoint = "https://notify-api.line.me/api/notify"

reqLineNotify :: B.ByteString -> B.ByteString -> Request
reqLineNotify message token = setRequestMethod "POST"
    $ setRequestBearerAuth token
    $ setRequestQueryString [("message", Just message)]
    $ parseRequest_ lineNotifyAPIEndPoint

notifyLine :: String -> IO Int
notifyLine message = getEnv "LINE_NOTIFY_TOKEN"
    >>= httpLbs . reqLineNotify (BU.fromString message) . BC.pack
    <&> getResponseStatusCode

messageDoc :: String -> String -> OA.Doc
messageDoc prURL artifactsURL = mconcat [
    OA.line
  , OA.text "🐥 roki-web PR Artifacts 🐥"
  , OA.line
  , OA.line <> OA.text "・PR: " <> OA.text prURL
  , OA.line <> OA.text "・Artifacts: " <> OA.text artifactsURL
  ]

data Opts = Opts
  { optPRURL        :: String
  , optArtifactsURL :: String
  }

pPRURL :: OA.Parser String
pPRURL = OA.option OA.str $ mconcat [
    OA.long "pr-url"
  , OA.help "A Pull request URL"
  , OA.metavar "<URL>"
  ]

pArtifactsURL :: OA.Parser String
pArtifactsURL = OA.option OA.str $ mconcat [
    OA.long "artifacts-url"
  , OA.help "A Circle CI Artifacts URL"
  , OA.metavar "<URL>"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> pPRURL
    <*> pArtifactsURL

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "A script that tells line that the roki-web artifacts build is complete"
  ]

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    notifyLine (show $ messageDoc (optPRURL opts) (optArtifactsURL opts))
        >>= print
