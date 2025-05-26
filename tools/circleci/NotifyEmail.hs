#!/usr/bin/env stack
-- stack script --resolver lts-22.33 --package bytestring --package smtp-mail --package mime-mail --package optparse-applicative --package text

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.String                     (IsString (..))
import qualified Data.Text.Lazy                  as LT
import           Network.Mail.Mime               (simpleMail')
import           Network.Mail.SMTP               (sendMailWithLoginOAuthSTARTTLS)
import qualified Options.Applicative             as OA
import qualified Options.Applicative.Help.Pretty as OA
import           System.Environment              (getEnv)

messageText :: LT.Text -> LT.Text -> LT.Text
messageText prURL artifactsURL = LT.unlines [
    LT.pack "🐥 roki-web PR Artifacts 🐥\n"
  , LT.pack "・PR: " <> prURL
  , LT.pack "・Artifacts: " <> artifactsURL
  ]

data Opts = Opts
  { optPRURL        :: LT.Text
  , optArtifactsURL :: LT.Text
  , optFromEmail    :: LT.Text
  , optToEmail      :: LT.Text
  }

pPRURL :: OA.Parser LT.Text
pPRURL = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "pr-url"
  , OA.help "A Pull request URL"
  , OA.metavar "<URL>"
  ]

pArtifactsURL :: OA.Parser LT.Text
pArtifactsURL = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "artifacts-url"
  , OA.help "A Circle CI Artifacts URL"
  , OA.metavar "<URL>"
  ]

pFromEmail :: OA.Parser LT.Text
pFromEmail = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "from-email"
  , OA.help "Sender email address"
  , OA.metavar "<EMAIL>"
  ]

pToEmail :: OA.Parser LT.Text
pToEmail = OA.option (LT.pack <$> OA.str) $ mconcat [
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

sendEmail :: LT.Text -> LT.Text -> LT.Text -> String -> IO ()
sendEmail message fromEmail toEmail token = sendMailWithLoginOAuthSTARTTLS "smtp.gmail.com" (LT.unpack fromEmail) token mail
  where
    mail = simpleMail'
      (fromString $ LT.unpack toEmail)  -- to
      (fromString $ LT.unpack fromEmail)  -- from
      "roki-web PR Artifacts Notification"  -- subject
      message  -- body

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    let message = messageText (optPRURL opts) (optArtifactsURL opts) in
      getEnv "GMAIL_APP_PASSWORD"
        >>= sendEmail message (optFromEmail opts) (optToEmail opts)
        >> putStrLn "Email sent successfully"
