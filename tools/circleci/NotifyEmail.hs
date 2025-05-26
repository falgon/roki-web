#!/usr/bin/env stack
-- stack script --resolver lts-22.33 --package bytestring --package smtp-mail --package mime-mail --package optparse-applicative --package text

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text                       as T
import           Network.Mail.Mime               (simpleMail')
import           Network.Mail.SMTP               (sendMailWithLoginOAuthSTARTTLS)
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
  , optFromEmail    :: String
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

pFromEmail :: OA.Parser String
pFromEmail = OA.option OA.str $ mconcat [
    OA.long "from-email"
  , OA.help "Sender email address"
  , OA.metavar "<EMAIL>"
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
    <*> pFromEmail
    <*> pToEmail

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "A script that sends an email notification about roki-web artifacts build completion"
  ]

sendEmail :: String -> String -> String -> String -> IO ()
sendEmail message fromEmail toEmail token = do
    let mail = simpleMail' (fromString toEmail) [fromString fromEmail] "roki-web PR Artifacts Notification" message
    sendMailWithLoginOAuthSTARTTLS "smtp.gmail.com" fromEmail token mail

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    let message = show $ messageDoc (optPRURL opts) (optArtifactsURL opts) in
      getEnv "GMAIL_APP_PASSWORD"
        >>= sendEmail message (optFromEmail opts) (optToEmail opts)
        >> putStrLn "Email sent successfully"
