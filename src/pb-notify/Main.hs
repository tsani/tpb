{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Misc
import Network.Pushbullet.Types

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad ( forever, forM_ )
import Data.Aeson ( eitherDecode' )
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( (<>) )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Libnotify as Noti
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import qualified Network.WebSockets as WS
import Servant.Client
import System.Environment ( getEnv )
import System.Exit ( exitFailure )
import System.Timeout ( timeout )
import Wuss ( runSecureClient )

appName :: String
appName = "pb-notify"

timeoutDelay :: Int
timeoutDelay = 5000000 -- five seconds

main :: IO ()
main = do
  token <- getEnv "PUSHBULLET_KEY"
  chan <- newChan
  _ <- forkIO (http chan (PushbulletKey (T.pack token)))
  forever $ do
    a <- async $ do
      runSecureClient
        "stream.pushbullet.com"
        443
        ("/websocket/" <> token)
        (ws chan)
    waitCatch a >>= \case
      Left err -> print err
      Right () -> pure ()
    threadDelay timeoutDelay
    putStrLn "restarting websocket connection..."

http :: Chan () -> PushbulletKey -> IO ()
http chan key = do
  let auth = pushbulletAuth key

  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv manager url
  let runClient = {- debug -} retryingDelay timeoutDelay . flip runClientM env

  -- create a variable holding the last push time we've processed.
  -- Initially, we set it to contain the time of the most recent push, if one
  -- exists; else, we set it to UTC zero.
  lastPushTimeVar <- do
    -- get the most recent push, just for its timestamp
    (Page (ExistingPushes pushes) _) <-
      runClient (getPushes auth Nothing (Just True) (Just 1) Nothing)
    newIORef (maybe minPushbulletTime pushModified (listToMaybe pushes))

  forever $ do
    -- block until a new request
    _ <- readChan chan
    lastPushTime <- readIORef lastPushTimeVar
    let f = fmap (fmap unExistingPushes)
    let getPushes' time active n = f . runClient . getPushes auth time active n
    let getPushes'' = getPushes' (Just lastPushTime) (Just True) Nothing
    start <- getPushes'' Nothing
    let next = getPushes'' . Just
    pushes <- sortBy (comparing pushModified)
      <$> getPaginatedLimit All start next
    forM_ pushes $ \Push{ pushData, pushModified } -> do
      writeIORef lastPushTimeVar pushModified

      Noti.display_ . mconcat $ case pushData of
        NotePush{..} -> let title = maybe "[untitled]" T.unpack pushTitle in
          [ Noti.summary ("Note: " ++ title)
          , Noti.body (T.unpack pushBody)
          , Noti.appName appName
          ]
        LinkPush{..} -> let title = maybe "[untitled]" T.unpack pushTitle in
          [ Noti.summary ("Link: " ++ title)
          , Noti.body (T.unpack pushBody)
          , Noti.appName appName
          ]
        FilePush{..} ->
          [ Noti.summary ("File: " ++ T.unpack pushFileName)
          , Noti.body (T.unpack (unUrl pushFileUrl))
          , Noti.appName appName
          ]

ws :: Chan () -> WS.ClientApp ()
ws chan connection = do
  rawm <- timeout 35000000 (WS.receiveData connection)
  case rawm of
    Nothing -> putStrLn "websocket receive timed out"
    Just raw -> handle chan raw *> ws chan connection

handle :: Chan () -> C8.ByteString -> IO ()
handle chan raw = do
  let message = eitherDecode' (LBS.fromStrict raw)
  case message of
    Right x -> case x :: Ephemeral of
      Tickle t -> case t of
        PushType -> writeChan chan ()
        OtherType t' -> putStrLn $ "got other tickle: " ++ T.unpack t'
      PushEphemeral p -> case p of
        SmsChanged{..} ->
          forM_ _ephNotifications $ \Notification{..} -> do
            t <- niceTime _notifTime
            Noti.display_ $ mconcat
              [ Noti.summary (T.unpack $ "SMS from " <> _notifTitle)
              , Noti.body (T.unpack (_notifBody <> "\n") <> t)
              , Noti.appName appName
              ]
        _ -> pure ()
      _ -> pure ()
    Left _ -> pure ()

niceTime :: PushbulletTime -> IO String
niceTime (PushbulletTime t) =
  formatTime defaultTimeLocale "%a %d %b %Y @ %H:%M:%S"
    <$> (utcToLocalTime <$> getTimeZone t <*> pure t)

-- | Retries an IO action that can fail with Either indefinitely.
retrying :: IO (Either e a) -> IO a
retrying m = either (const (retrying m)) pure =<< m

-- | Retries an IO action that can fail with Either by delaying a given number
-- of microseconds before retrying, indefinitely.
retryingDelay :: Show e => Int -> IO (Either e a) -> IO a
retryingDelay n m = either loop pure =<< m where
  loop e = do
    putStrLn $ "retrying... " ++ show e
    threadDelay n
    retryingDelay n m

debug :: Show e => IO (Either e a) -> IO a
debug m = do
  a <- m
  case a of
    Left err -> print err *> exitFailure
    Right x -> pure x
