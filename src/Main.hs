{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Command
import Format
import Request
import ResponseFormat
import Sum

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Misc
import Network.Pushbullet.Types

import Control.Monad.Free
import Data.Bifunctor ( first )
import Data.ByteString ( readFile )
import qualified Data.ByteString as BS
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Prelude hiding ( readFile )
import Options.Applicative
import Servant.Client
import System.Exit ( exitFailure )
import System.IO ( stderr, hPutStrLn )

main :: IO ()
main
  = execParser opts
    -- parse commandline options, putting IO actions to be executed later in
    -- place of missing values
  >>= sequence
    -- load all the defaults for real by sequencing those actions.
  >>= run
    -- execute the request

opts :: ParserInfo (Request IO (IO PushbulletKey))
opts = fullDescInfo cliRequest

cliRequest :: Parser (Request IO (IO PushbulletKey))
cliRequest
  = pure Request
  <*> flag
    (ExistsRenderableFormat <$> formatHumanTable)
    (ExistsRenderableFormat . pure <$> formatJsv)
    (long "jsv")
  <*> option
    (fmap pure $ PushbulletKey <$> raw)
    (long "key" <> value (PushbulletKey <$> line access))
  <*> subparser (
    command "sms" (
      fullDescInfo $ subparser (
        command "list" (
          fullDescInfo $
            pure (\d t -> do
              d' <- maybe (DeviceId <$> line device) pure d
              pure $ inject <$> listSms d' t
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> option (SmsThreadId <$> raw) (long "thread")
        )
        <>
        command "send" (
          fullDescInfo $
            pure (\d n m -> do
              d' <- maybe (fmap DeviceId $ line device) pure d
              pure $ inject <$> do
                User {userId = u} <- me
                sendSms u d' n m
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> option (PhoneNumber <$> raw) (long "number")
            <*> option raw (long "message")
        )
        <>
        command "threads" (
          fullDescInfo $
            pure (\d n -> do
              d' <- maybe (fmap DeviceId $ line device) pure d
              pure $ inject <$> do
                filter (matchRecipientName n) <$> listThreads d'
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> optional (option (Name <$> raw) (long "involving"))
        )
      )
    )
    <>
    command "devices" (
      fullDescInfo $ subparser (
        command "list" (
          fullDescInfo $
            pure (\c -> do
              let c' = maybe All id c
              pure $ inject <$> listDevices c'
            )
            <*> optional (option (Limit <$> auto) (long "limit"))
        )
      )
    )
  )

-- | Execute a request.
run :: Request IO PushbulletKey -> IO ()
run (Request format key cmd) = do
  -- prepare the command
  cmd' <- cmd

  -- get ready to do some motherfucking http requests
  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  -- let url = BaseUrl Http "localhost" 8088 ""
  let env = ClientEnv manager url

  let comm = httpCommand key cmd'
  response <- fmap (first ServantError) . flip runClientM env $ comm

  case response of
    Left e -> do
      ePutStrLn (show e)
      exitFailure
    Right r -> case match' format r of
      ExistsRenderableFormat f -> printFormat =<< f

-- | Interprets a computation in the 'Command' monad into a computation in the
-- 'ClientM' monad that actually performs HTTP requests.
httpCommand :: PushbulletKey -> Command a -> ClientM a
httpCommand key = iterM phi where
  auth = pushbulletAuth key

  phi :: CommandF (ClientM a) -> ClientM a
  phi com = case com of
    ListSms d t k ->
      k . unSmsMessages =<< getSmsMessages auth (d `MessagesIn` t)

    ListThreads d k ->
      k . unSmsThreads =<< getSmsThreads auth (ThreadsOf d)

    SendSms u d n m k -> createEphemeral auth (Sms u d n m) *> k

    ListDevices count k -> do
      let getDevices' a = fmap (fmap unExistingDevices) . getDevices auth a
      start <- getDevices' Nothing Nothing
      k =<< getPaginatedLimit count start (getDevices' Nothing . Just)

    Me k -> k =<< getMe auth

fullDescInfo :: Parser a -> ParserInfo a
fullDescInfo p = info (helper <*> p) fullDesc

raw :: ReadM T.Text
raw = T.pack <$> str

line :: FilePath -> IO T.Text
line p = decodeUtf8 . BS.init <$> readFile p

device :: String
device = "/home/tsani/.phoneDeviceID"

access :: String
access = "/home/tsani/.pushbulletaccess"

type Request'
  = Request IO PushbulletKey

data Error
  = ServantError ServantError
  deriving (Eq, Show)

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- | Match a name to an sms thread. If there is no name given, always produces
-- True. Otherwise, returns True if and only if there exists a recipient of the
-- thread whose casefolded name contains the given name as a substring.
matchRecipientName :: Maybe Name -> SmsThread -> Bool
matchRecipientName Nothing = const True
matchRecipientName (Just (Name (T.toCaseFold -> n)))
  = any ((n `T.isInfixOf`) . T.toCaseFold . unName . recipientName)
  . threadRecipients
