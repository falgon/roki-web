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

messageText :: T.Text -> T.Text -> T.Text
messageText prURL artifactsURL = T.unlines [
    T.pack "🐥 roki-web PR Artifacts 🐥\n"
  , T.pack "・PR: " <> prURL
  , T.pack "・Artifacts: " <> artifactsURL
  ]

data Opts = Opts
  { optPRURL        :: T.Text
  , optArtifactsURL :: T.Text
  , optFromEmail    :: T.Text
  , optToEmail      :: T.Text
  }

pPRURL :: OA.Parser T.Text
pPRURL = OA.option (T.pack <$> OA.str) $ mconcat [
    OA.long "pr-url"
  , OA.help "A Pull request URL"
  , OA.metavar "<URL>"
  ]

pArtifactsURL :: OA.Parser T.Text
pArtifactsURL = OA.option (T.pack <$> OA.str) $ mconcat [
    OA.long "artifacts-url"
  , OA.help "A Circle CI Artifacts URL"
  , OA.metavar "<URL>"
  ]

pFromEmail :: OA.Parser T.Text
pFromEmail = OA.option (T.pack <$> OA.str) $ mconcat [
    OA.long "from-email"
  , OA.help "Sender email address"
  , OA.metavar "<EMAIL>"
  ]

pToEmail :: OA.Parser T.Text
pToEmail = OA.option (T.pack <$> OA.str) $ mconcat [
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

sendEmail :: T.Text -> T.Text -> T.Text -> String -> IO ()
sendEmail message fromEmail toEmail token = sendMailWithLoginOAuthSTARTTLS "smtp.gmail.com" (T.unpack fromEmail) token mail
  where
    mail = simpleMail'
      (fromString $ T.unpack toEmail)    -- to
      [fromString $ T.unpack fromEmail]  -- from
      "roki-web PR Artifacts Notification"  -- subject
      message  -- body

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    let message = messageText (optPRURL opts) (optArtifactsURL opts) in
      getEnv "GMAIL_APP_PASSWORD"
        >>= sendEmail message (optFromEmail opts) (optToEmail opts)
        >> putStrLn "Email sent successfully"
