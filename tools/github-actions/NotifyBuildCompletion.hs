#!/usr/bin/env stack
-- stack script --resolver lts-22.33 --package bytestring --package smtp-mail --package mime-mail --package optparse-applicative --package text

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception   (SomeException, catch)
import           Data.String         (IsString (..))
import qualified Data.Text.Lazy      as LT
import           Network.Mail.Mime   (simpleMail')
import           Network.Mail.SMTP   (sendMailWithLoginSTARTTLS)
import qualified Options.Applicative as OA
import           System.Environment  (getEnv, lookupEnv)
import           System.Exit         (exitFailure, exitSuccess)

messageText :: LT.Text -> LT.Text -> LT.Text -> LT.Text -> LT.Text
messageText repository imageTag runId actionsURL = LT.unlines [
    LT.pack "🐥 roki.dev Docker Image Build Completed 🐥\n"
  , LT.pack "Docker image build for roki.dev has been completed successfully.\n"
  , LT.pack "・Repository: " <> repository
  , LT.pack "・Image Tag: " <> imageTag
  , LT.pack "・Run ID: " <> runId
  , LT.pack "・Actions URL: " <> actionsURL
  , LT.pack ""
  , LT.pack "This is an automated notification from GitHub Actions."
  ]

data Opts = Opts
  { optRepository :: LT.Text
  , optImageTag   :: LT.Text
  , optRunId      :: LT.Text
  , optActionsURL :: LT.Text
  , optFromEmail  :: Maybe LT.Text
  , optToEmail    :: Maybe LT.Text
  , optSubject    :: LT.Text
  }

pRepository :: OA.Parser LT.Text
pRepository = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "repository"
  , OA.help "GitHub repository (owner/name)"
  , OA.metavar "<REPO>"
  , OA.value "falgon/roki-web"
  , OA.showDefault
  ]

pImageTag :: OA.Parser LT.Text
pImageTag = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "image-tag"
  , OA.help "Docker image tag"
  , OA.metavar "<TAG>"
  , OA.value "latest"
  , OA.showDefault
  ]

pRunId :: OA.Parser LT.Text
pRunId = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "run-id"
  , OA.help "GitHub Actions run ID"
  , OA.metavar "<ID>"
  ]

pActionsURL :: OA.Parser LT.Text
pActionsURL = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "actions-url"
  , OA.help "GitHub Actions run URL"
  , OA.metavar "<URL>"
  ]

pFromEmail :: OA.Parser (Maybe LT.Text)
pFromEmail = OA.optional $ OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "from-email"
  , OA.help "Sender email address (overrides GMAIL_SENDER env var)"
  , OA.metavar "<EMAIL>"
  ]

pToEmail :: OA.Parser (Maybe LT.Text)
pToEmail = OA.optional $ OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "to-email"
  , OA.help "Recipient email address (overrides NOTIFICATION_TO_EMAIL env var)"
  , OA.metavar "<EMAIL>"
  ]


pSubject :: OA.Parser LT.Text
pSubject = OA.option (LT.pack <$> OA.str) $ mconcat [
    OA.long "subject"
  , OA.help "Email subject"
  , OA.metavar "<SUBJECT>"
  , OA.value "roki.dev Docker Image Build Completed"
  , OA.showDefault
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> pRepository
    <*> pImageTag
    <*> pRunId
    <*> pActionsURL
    <*> pFromEmail
    <*> pToEmail
    <*> pSubject

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.header "NotifyBuildCompletion - GitHub Actions email notification tool"
  , OA.progDesc "Sends email notification about Docker image build completion"
  ]

getEmailConfig :: Opts -> IO (LT.Text, LT.Text, String)
getEmailConfig opts = do
    -- 送信元メールアドレス
    fromEmail <- case optFromEmail opts of
        Just email -> return email
        Nothing -> do
            envFrom <- lookupEnv "GMAIL_SENDER"
            case envFrom of
                Just email -> return $ LT.pack email
                Nothing -> do
                    putStrLn "Error: Sender email not provided. Use --from-email or set GMAIL_SENDER"
                    exitFailure

    -- 送信先メールアドレス
    toEmail <- case optToEmail opts of
        Just email -> return email
        Nothing -> do
            envTo <- lookupEnv "NOTIFICATION_TO_EMAIL"
            case envTo of
                Just email -> return $ LT.pack email
                Nothing -> do
                    putStrLn "Error: Recipient email not provided. Use --to-email or set NOTIFICATION_TO_EMAIL"
                    exitFailure

    -- Gmailアプリパスワード
    password <- do
        envPass <- lookupEnv "GMAIL_APP_PASSWORD"
        case envPass of
            Just pass -> return pass
            Nothing -> do
                putStrLn "Error: Gmail app password not found. Set GMAIL_APP_PASSWORD"
                exitFailure

    return (fromEmail, toEmail, password)

sendEmail :: LT.Text -> LT.Text -> LT.Text -> LT.Text -> String -> IO ()
sendEmail subject message fromEmail toEmail password =
    sendMailWithLoginSTARTTLS "smtp.gmail.com" (LT.unpack fromEmail) password mail
  where
    mail = simpleMail'
      (fromString $ LT.unpack toEmail)  -- to
      (fromString $ LT.unpack fromEmail)  -- from
      (LT.toStrict subject)  -- subject
      message  -- body

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    (fromEmail, toEmail, password) <- getEmailConfig opts

    let message = messageText
            (optRepository opts)
            (optImageTag opts)
            (optRunId opts)
            (optActionsURL opts)

    catch (do
        sendEmail (optSubject opts) message fromEmail toEmail password
        putStrLn $ "✅ Email sent successfully (To: " ++ LT.unpack toEmail ++ ")"
      ) (\e -> do
        let err = show (e :: SomeException)
        putStrLn $ "❌ Failed to send email: " ++ err
        exitFailure
      )
