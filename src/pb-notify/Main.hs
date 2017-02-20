{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Pushbullet.Types

import Control.Monad ( forever, forM_ )
import Data.Aeson ( eitherDecode' )
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Libnotify as Noti
import qualified Network.WebSockets as WS
import System.Environment ( getEnv )
import Wuss ( runSecureClient )

main :: IO ()
main = do
  token <- getEnv "PUSHBULLET_KEY"
  runSecureClient "stream.pushbullet.com" 443 ("/websocket/" <> token) ws

ws :: WS.ClientApp ()
ws connection = forever $ do
  raw <- WS.receiveData connection
  let message = eitherDecode' (LBS.fromStrict raw)
  case message of
    Left _ -> pure ()
    Right x -> case x of
      SmsChanged{..} ->
        forM_ _ephNotifications $ \Notification{..} -> do
          t <- niceTime _notifTime
          Noti.display_ $
            Noti.summary (T.unpack $ "SMS from " <> _notifTitle) <>
            Noti.body (
              T.unpack (_notifBody <> "\n") <> t
            )
      _ -> pure ()

niceTime :: PushbulletTime -> IO String
niceTime (PushbulletTime t) =
  formatTime defaultTimeLocale "%a %d %b %Y @ %H:%M:%S"
    <$> (utcToLocalTime <$> getTimeZone t <*> pure t)

