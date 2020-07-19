{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types

import Control.Concurrent ( threadDelay )
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import qualified Network.WebSockets as WS
import Servant.Client
import System.Environment ( getEnv )
import Wuss ( runSecureClient )

newtype f :~> g = Nat { runNat :: forall a. f a -> g a }

streamUrl :: String
streamUrl = "stream.pushbullet.com"

apiUrl :: BaseUrl
apiUrl = BaseUrl Https "api.pushbullet.com" 443 ""

data Env
  = Env
    { runClient :: ClientM :~> IO
    , auth :: Auth
    , userId :: UserId
    , deviceId :: DeviceId
    , wsconnection :: WS.Connection
    }

newtype PBD a = PBD { unPBD :: ReaderT Env IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadIO
    )

runPBD :: Env -> PBD a -> IO a
runPBD env (PBD m) = runReaderT m env

-- | Constructs a remote directory request ephemeral to be sent over the websocket.
rdr :: Env -> FilePath -> Ephemeral
rdr Env{userId, deviceId} _ephRdrPath =
  PushEphemeral (Just allEphemeralTargets) $
  RemoteDirectoryRequest
    { _ephRdrPath
    , _ephRdrSourceUser = userId
    , _ephRdrTargetDevice = deviceId
    }

-- | Receives websocket messages until one conforming to the given
-- transformation is received.
awaitWebsocketResponse :: MonadPBD m => (LBS.ByteString -> Maybe b) -> m b
awaitWebsocketResponse f = do
  x <- recvWS
  case f x of
    Just y -> pure y
    Nothing -> do
      liftIO $ do
        putStrLn "Got bogus response"
        T.putStr $ T.decodeUtf8 (LBS.toStrict x)
        putStrLn ""
      awaitWebsocketResponse f

class (Monad m, MonadIO m) => MonadPBD m where
  sendWS :: WS.WebSocketsData a => a -> m ()
  recvWS :: WS.WebSocketsData a => m a
  remoteDirectoryRequest :: FilePath -> m [RemoteDirectoryEntry]
  authenticate :: (Auth -> a) -> m a
  http :: ClientM a -> m a

instance MonadPBD PBD where
  sendWS x = do
    ws <- asks wsconnection
    liftIO $ WS.sendBinaryData ws x

  recvWS = do
    ws <- asks wsconnection
    liftIO $ WS.receiveData ws

  authenticate f = f <$> asks auth

  http m = runNat <$> asks runClient <*> pure m >>= liftIO

  remoteDirectoryRequest path = do
    env <- ask
    _ <- http =<< authenticate createEphemeral <*> pure (rdr env path)
    awaitWebsocketResponse $ \x -> do
      decode x >>= \case
        -- TODO check that device ID and path match request
        PushEphemeral _ (RemoteDirectory { _ephRdContents }) -> Just _ephRdContents
        _ -> Nothing

main :: IO ()
main = do
  token <- getEnv "PUSHBULLET_KEY"
  deviceId <- DeviceId . T.pack <$> getEnv "PBNOTIFY_DEVICE"

  let key = PushbulletKey (T.pack token)

  let auth = pushbulletAuth key

  manager <- newManager tlsManagerSettings
  let clientEnv = ClientEnv { manager, baseUrl = apiUrl, cookieJar = Nothing }
  let runClient = Nat (retryingDelay timeoutDelay . flip runClientM clientEnv)

  userId <- _userId <$> runNat runClient (getMe auth)
  -- TODO send a ping to the target device to wake it up before trying to send the RDR

  runSecureClient streamUrl 443 ("/websocket/" <> token) $ \wsconnection -> do
    WS.withPingThread wsconnection 30 (pure ()) $ do
      entries <- runPBD Env {..} $ remoteDirectoryRequest "~"
      forM_ entries print

timeoutDelay = 5000000

-- | Retries an IO action that can fail with Either by delaying a given number
-- of microseconds before retrying, indefinitely.
retryingDelay :: Show e => Int -> IO (Either e a) -> IO a
retryingDelay n m = either loop pure =<< m where
  loop e = do
    putStrLn $ "retrying... " ++ show e
    threadDelay n
    retryingDelay n m
