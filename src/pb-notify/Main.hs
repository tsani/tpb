{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Maybe ( listToMaybe )
import Data.Monoid ( (<>) )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Libnotify as Noti
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Network.Wai ( Application )
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

appName :: String
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

pbNotifyApi :: Proxy PbNotifyApi
pbNotifyApi = Proxy

addNoti
  :: Noti.Mod Noti.Notification
  -> T.Text
  -> NotifyMap
  -> IO NotifyMap
addNoti n t (NotifyMap m)= do
  noti <- Noti.display n
  pure $ NotifyMap (M.insert t noti m)

deleteNoti :: T.Text -> NotifyMap -> IO NotifyMap
deleteNoti t (NotifyMap m) = do
  case M.lookup t m of
    Nothing -> pure (NotifyMap m)
    Just n -> do
      Noti.close n
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
  token <- getEnv "PUSHBULLET_KEY"
  deviceIdS <- getEnv "PBNOTIFY_DEVICE"
  listenPort <- do
    p <- getEnv "PBNOTIFY_PORT"
    maybe (die "PBNOTIFY_PORT must be an integer") pure $ readMaybe p

  let key = PushbulletKey (T.pack token)

  -- strategy for making clipboard ephemerals
  makeClipEphemeral <- do
    uid <- _userId <$> getUser key
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
  _ <- async (http notiVar httpChan makeClipEphemeral key)

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

getUser :: PushbulletKey -> IO User
getUser key = do
  let auth = pushbulletAuth key

  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv manager url
  let runClient = {- debug -} retryingDelay timeoutDelay . flip runClientM env

  runClient (getMe auth)

http
  :: NotifyMapVar
  -> HttpChan
  -> (T.Text -> Ephemeral)
  -> PushbulletKey
  -> IO ()
http notiVar httpChan mke key = do
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
    r <- getHttpChan httpChan

    case r of
      SendClip t -> runClient (createEphemeral auth (mke t)) *> pure ()

      CheckPushes -> do
        lastPushTime <- readIORef lastPushTimeVar
        let f = fmap (fmap unExistingPushes)
        getPushes' <- pure $ \time active n ->
          f . runClient . getPushes auth time active n
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
            let note = preparePushNotification pushData
            let g = if pushActive then addNoti note else deleteNoti
            writeIORef v =<< g pid m

          (,) <$> readIORef v <*> pure ()

preparePushNotification :: PushData 'Existing -> Noti.Mod Noti.Notification
preparePushNotification pushData = mconcat $ case pushData of
  NotePush{..} ->
    [ Noti.summary ("Note: " ++ maybe "[untitled]" T.unpack pushTitle)
    , Noti.body (T.unpack pushBody)
    , Noti.appName appName
    ]
  LinkPush{..} ->
    [ Noti.summary ("Link: " ++ maybe "[untitled]" T.unpack pushTitle)
    , Noti.body $ T.unpack $ formatLinkPush pushUrl pushLinkBody
    , Noti.appName appName
    ]
  FilePush{..} ->
    [ Noti.summary ("File: " ++ T.unpack pushFileName)
    , Noti.body (T.unpack (unUrl pushFileUrl))
    , Noti.appName appName
    ]
  where
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
            Noti.display_ $ mconcat
              [ Noti.summary (T.unpack $ "SMS from " <> _notifTitle)
              , Noti.body (T.unpack (_notifBody <> "\n") <> t)
              , Noti.appName appName
              ]
        Clipboard{..} ->
          modifyMVar_ lock $ const $ do
            writeIORef clip _ephClipBody
            pure True
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
