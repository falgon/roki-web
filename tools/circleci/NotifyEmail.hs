#!/usr/bin/env stack
-- stack script --resolver lts-14.27 --package bytestring --package smtp-mail --package optparse-applicative --package text

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8           as BC
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import qualified Options.Applicative             as OA
import qualified Options.Applicative.Help.Pretty as OA
import           System.Environment              (getEnv)

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
  , optToEmail      :: String
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

pToEmail :: OA.Parser String
pToEmail = OA.option OA.str $ mconcat [
    OA.long "to-email"
  , OA.help "Recipient email address"
  , OA.metavar "<EMAIL>"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> pPRURL
    <*> pArtifactsURL
    <*> pToEmail

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "A script that sends an email notification about roki-web artifacts build completion"
  ]

sendEmail :: String -> String -> String -> IO ()
sendEmail message toEmail password = do
    let connSettings = SMTPConnectionSettings
            { smtpServer = "smtp.gmail.com"
            , smtpPort = 587
            , smtpSecurity = STARTTLS
            , smtpUsername = toEmail
            , smtpPassword = password
            }

    sendMail connSettings
        (T.pack toEmail)  -- from
        [T.pack toEmail]  -- to
        "roki-web PR Artifacts Notification"  -- subject
        (T.pack message)  -- body

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    let message = show $ messageDoc (optPRURL opts) (optArtifactsURL opts)
    getEnv "GMAIL_APP_PASSWORD" >>= sendEmail message (optToEmail opts)
    putStrLn "Email sent successfully"
