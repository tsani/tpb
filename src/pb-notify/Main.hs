{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Welcome to pb-notify's implementation.
-- The basic structure of this program is informed by how PushBullet a
-- websocket client.
-- Imagine a setup like the following, with clients Alice and Bob,
-- communicating with PushBullet.
--
-- > Alice --> PB <-- Bob
--
-- Bob is connected to PushBullet over a websocket connection. We say
-- Bob is "listening to the livestream".
-- Alice creates a push notification via an HTTP API call.
-- PushBullet will send a notification to all websocket subscribers,
-- such as Bob. The event received by Bob, however, does not contain
-- much information! Indeed, this is a so-called \"tickle\" event.
-- In response to a tickle event, pb-notify needs to see if any new
-- pushes have arrived, and transform any into system notifications.
-- To do so, pb-notify stores the time of the most recent push, so
-- that when asking PushBullet what new pushes there are, pb-notify
-- can use the appropriate query string parameter to control what
-- pushes get listed.
--
-- Furthermore, in order for PushBullet's universal clipboard feature
-- to work, this application also exposes an HTTP server. By calling
-- pb-notify's HTTP server, a program running on the same host as
-- pb-notify can request to set the global PushBullet clipboard.
-- Similarly, when universal clipboard change notifications are
-- received via the websocket, the websocket client thread sets the
-- internal variable that stores the current known state of the
-- universal clipboard.
--
-- To make this setup work, this application is multithreaded.
-- * The websocket client thread: This thread listens for websocket
--   events, such as tickles, and sends an asynchronous request to the
--   HTTP client thread to check for new pushes.
--   (This is the 'checkPushes' function.
-- * The HTTP client thread: This thread awaits requests from other
--   threads to perform PushBullet HTTP API calls.
-- * The HTTP server thread: This thread awaits requests from other
--   applications to get or set the shared PushBullet clipboard.

module Main where

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Misc
import Network.Pushbullet.Types

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad ( forever, forM_ )
import Control.Monad.IO.Class
import Data.Aeson ( eitherDecode' )
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List ( sortBy )
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified GI.Notify as Noti
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Network.Wai.Handler.Warp ( run )
import qualified Network.WebSockets as WS
import Servant
import Servant.Client
import System.IO ( hPutStrLn, stderr )
import System.Environment ( getEnv )
import System.Exit ( exitFailure )
import System.Timeout ( timeout )
import Text.Read ( readMaybe )
import Wuss ( runSecureClient )

appName :: T.Text
appName = "pb-notify"

timeoutDelay :: Int
timeoutDelay = 5000000 -- five seconds

-- | A map from text (identifiers in the PushBullet API) to libnotify
-- notification.
newtype NotifyMap
  = NotifyMap
    { unNotifyMap :: M.Map T.Text Noti.Notification }

newtype NotifyMapVar
  = NotifyMapVar
    { unNotifyMapVar :: MVar NotifyMap }

data ClipDataVar
  = ClipDataVar
    { clipDataLock :: MVar Bool
      -- ^ The lock is used to guard access to the IORef and to indicate
      -- whether the IORef contains meaningful data
    , clipData :: IORef T.Text
    }

newClipDataVar :: IO ClipDataVar
newClipDataVar = do
  v <- newMVar False
  r <- newIORef ""
  pure ClipDataVar { clipDataLock = v, clipData = r }

-- | Reads the clip data; if it's empty then Nothing is returned.
readClipVar :: ClipDataVar -> IO (Maybe T.Text)
readClipVar (ClipDataVar lock r) = withMVar lock $ \case
  True -> Just <$> readIORef r
  False -> pure Nothing

newtype HttpChan
  = HttpChan
    { unHttpChan :: Chan HttpReq }

-- | The types of things we can ask the HTTP client thread to do.
data HttpReq
  -- | Send the given text to the stream.
  = SendClip T.Text
  -- | Check for new pushes and convert to notifications.
  | CheckPushes

newHttpChan :: IO HttpChan
newHttpChan = HttpChan <$> newChan

checkPushes :: HttpChan -> IO ()
checkPushes (HttpChan c) = writeChan c CheckPushes

sendClip :: HttpChan -> T.Text -> IO ()
sendClip (HttpChan c) t = writeChan c (SendClip t)

getHttpChan :: HttpChan -> IO HttpReq
getHttpChan (HttpChan c) = readChan c

withNotifyMap :: NotifyMapVar -> (NotifyMap -> IO (NotifyMap, a)) -> IO a
withNotifyMap (NotifyMapVar v) m = do modifyMVar v m

type PbNotifyApi =
  "clip" :> (
    Get '[PlainText] T.Text
  :<|>
    ReqBody '[PlainText] T.Text :> PostAccepted '[PlainText] NoContent
  )

newtype f :~> g = Nat { runNat :: forall a. f a -> g a }

pbNotifyApi :: Proxy PbNotifyApi
pbNotifyApi = Proxy

addNoti
  :: Noti.Notification
  -> T.Text
  -> NotifyMap
  -> IO NotifyMap
addNoti n t (NotifyMap m)= do
  Noti.notificationShow n
  pure $ NotifyMap (M.insert t n m)

deleteNoti :: T.Text -> NotifyMap -> IO NotifyMap
deleteNoti t (NotifyMap m) = do
  case M.lookup t m of
    Nothing -> pure (NotifyMap m)
    Just n -> do
      Noti.notificationClose n
      pure $ NotifyMap (M.delete t m)

webapp :: ClipDataVar -> HttpChan -> Application
webapp clipvar httpChan = serve pbNotifyApi (server clipvar httpChan)

server :: ClipDataVar -> HttpChan -> Server PbNotifyApi
server (ClipDataVar lock var) httpChan = getClip :<|> postClip where
  getClip :: Handler T.Text
  getClip = do
    liftIO (takeMVar lock) >>= \case
      False -> do
        liftIO (putMVar lock False)
        throwError err404 { errBody = "no clipboard data" }
      True -> do
        t <- liftIO (readIORef var)
        liftIO (putMVar lock True)
        pure t

  postClip :: T.Text -> Handler NoContent
  postClip t = do
    liftIO $ modifyMVar_ lock $ \b -> do
      -- writeIORef var t
      sendClip httpChan t
      -- pure True
      pure b
    pure NoContent

streamUrl :: String
streamUrl = "stream.pushbullet.com"

apiUrl :: String
apiUrl = "api.pushbullet.com"

die :: String -> IO a
die s = hPutStrLn stderr s *> exitFailure

main :: IO ()
main = do
  True <- Noti.init appName
  token <- getEnv "PUSHBULLET_KEY"
  deviceIdS <- getEnv "PBNOTIFY_DEVICE"
  listenPort <- getEnv "PBNOTIFY_PORT" >>=
    maybe (die "PBNOTIFY_PORT must be an integer") pure . readMaybe

  let key = PushbulletKey (T.pack token)

  let auth = pushbulletAuth key

  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv { manager, baseUrl = url, cookieJar = Nothing }
  let runClient = Nat (retryingDelay timeoutDelay . flip runClientM env)

  -- strategy for making clipboard ephemerals
  makeClipEphemeral <- do
    uid <- _userId <$> runNat runClient (getMe auth)
    let did = DeviceId (T.pack deviceIdS)

    pure $ \x -> PushEphemeral (Just allEphemeralTargets) Clipboard
      { _ephClipBody = x
      , _ephClipSourceUser = uid
      , _ephClipSourceDevice = did
      }

  httpChan <- newHttpChan
  clipvar <- newClipDataVar

  notiVar <- NotifyMapVar <$> newMVar (NotifyMap M.empty)

  -- run the asynchronous http thread
  -- This thread gets poked over the chan whenever a Tickle event is received
  -- over the websocket. This causes the list of pushes to get updated, and
  -- transformed into notifications.
  -- HTTP requests sent by this thread are retried indefinitely until they
  -- succeed.
  _ <- async (http runClient notiVar httpChan makeClipEphemeral auth)

  -- run the http server thread
  -- This thread runs a dead simple API to access the pushbullet clipboard
  -- POST requests to /clip will set the internal variable holding the PB
  -- clipboard and send the clipboard ephemeral over the websocket so other
  -- devices will have their clipboards set.
  -- GET requests will retrieve the value of the internal variable holding the
  -- PB clipboard. If no clipboard data has been received yet via an ephemeral
  -- on the websocket, this produces a 404.
  -- Note that the local clipboard is unaffected by these actions!
  _ <- async (run listenPort (webapp clipvar httpChan))

  let wsclient = runSecureClient streamUrl 443 ("/websocket/" <> token)

  forever $ do
    a <- async $ do
      wsclient (ws httpChan clipvar)

    waitCatch a >>= \case
      Left err -> print err
      Right () -> pure ()

    threadDelay timeoutDelay
    putStrLn "restarting websocket connection..."

http
  :: ClientM :~> IO
  -> NotifyMapVar
  -> HttpChan
  -> (T.Text -> Ephemeral)
  -> Auth
  -> IO ()
http runClient notiVar httpChan mke auth = do
  -- create a variable holding the last push time we've processed.
  -- Initially, we set it to contain the time of the most recent push, if one
  -- exists; else, we set it to UTC zero.
  lastPushTimeVar <- do
    -- get the most recent push, just for its timestamp
    (Page (ExistingPushes pushes) _) <-
      runNat runClient (getPushes auth Nothing (Just True) (Just 1) Nothing)
    newIORef (maybe minPushbulletTime pushModified (listToMaybe pushes))

  forever $ do
    -- block until a new request
    getHttpChan httpChan >>= \case
      SendClip t -> runNat runClient (createEphemeral auth (mke t)) *> pure ()

      CheckPushes -> do
        lastPushTime <- readIORef lastPushTimeVar
        let f = fmap (fmap unExistingPushes)
        getPushes' <- pure $ \time active n ->
          f . runNat runClient . getPushes auth time active n
        let getPushes'' = getPushes' (Just lastPushTime) (Just True) Nothing
        start <- getPushes'' Nothing
        let next = getPushes'' . Just
        pushes <- sortBy (comparing pushModified)
          <$> getPaginatedLimit All start next

        withNotifyMap notiVar $ \notiMap -> do
          v <- newIORef notiMap

          forM_ pushes $ \Push{..} -> do
            let (PushId pid) = pushId
            writeIORef lastPushTimeVar pushModified

            m <- readIORef v
            note <- preparePushNotification pushData
            let g = if pushActive && not pushDismissed then addNoti note else deleteNoti
            writeIORef v =<< g pid m

          (,) <$> readIORef v <*> pure ()

preparePushNotification :: PushData 'Existing -> IO Noti.Notification
preparePushNotification pushData = case pushData of
  NotePush{..} ->
    prep
      ("Note: " <> fromMaybe "[untitled]" pushTitle)
      pushBody
  LinkPush{..} ->
    prep
      ("Link: " <> fromMaybe "[untitled]" pushTitle)
      (formatLinkPush pushUrl pushLinkBody)
  FilePush{..} ->
    prep ("File: " <> pushFileName) (unUrl pushFileUrl)
  where
    prep summary body = Noti.notificationNew summary (Just body) Nothing
    formatLinkPush (Url url) mbody = case mbody of
      Nothing -> url
      Just body -> body <> "\n" <> url

ws
  :: HttpChan
  -- ^ The channel used to wake up the HTTP thread to check for new pushes
  -> ClipDataVar
  -- ^ The variable to store our clipboard buffer in
  -> WS.ClientApp ()
ws httpChan clipvar connection = recv where
  recv :: IO ()
  recv = do
    -- this timeout is set to 35s. Pushbullet is supposed to send us a
    -- heartbeat every 30s.
    rawm <- timeout 35000000 (WS.receiveData connection)
    case rawm of
      Nothing -> putStrLn "websocket receive timed out"
      Just raw -> handle httpChan clipvar raw *> recv

handle :: HttpChan -> ClipDataVar -> C8.ByteString -> IO ()
handle httpChan (ClipDataVar lock clip) raw = do
  let message = eitherDecode' (LBS.fromStrict raw)
  case message of
    Right x -> case x :: Ephemeral of
      Tickle t -> case t of
        PushType -> checkPushes httpChan
        OtherType t' -> putStrLn $ "got other tickle: " ++ T.unpack t'
      PushEphemeral _ p -> case p of
        SmsChanged{..} ->
          forM_ _ephNotifications $ \Notification{..} -> do
            t <- niceTime _notifTime
            n <- Noti.notificationNew
              ("SMS from " <> _notifTitle)
              (Just $ (_notifBody <> "\n" <> t))
              Nothing
            Noti.notificationShow n

        Clipboard{..} ->
          modifyMVar_ lock $ const $ do
            writeIORef clip _ephClipBody
            pure True
        _ -> pure ()
      _ -> pure ()
    Left _ -> pure ()

niceTime :: PushbulletTime -> IO T.Text
niceTime (PushbulletTime t) =
  T.pack . formatTime defaultTimeLocale "%a %d %b %Y @ %H:%M:%S"
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
