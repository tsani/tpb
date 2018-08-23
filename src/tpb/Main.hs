{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad.Except
import Control.Monad.Free ( iterM )
import Data.Bool ( bool )
import Data.ByteString ( readFile )
import qualified Data.ByteString as BS
import Data.List.NonEmpty ( toList )
import qualified Data.List.NonEmpty as N
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.LocalTime ( TimeZone, getTimeZone )
import Lens.Micro
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Options.Applicative
import Prelude hiding ( readFile )
import Servant.Client
import System.Environment ( getEnv )
import System.Exit ( exitFailure )
import System.IO ( stderr, hPutStrLn )

main :: IO ()
main
  = (execParser . opts =<< getTimeZone =<< getCurrentTime)
    -- parse commandline options, putting IO actions to be executed later in
    -- place of missing values
  >>= sequence
    -- load all the defaults for real by sequencing those actions.
  >>= run
    -- execute the request

opts :: TimeZone -> ParserInfo (Request IO (IO PushbulletKey))
opts = fullDescInfo . cliRequest

cliRequest :: TimeZone -> Parser (Request IO (IO PushbulletKey))
cliRequest tz
  = pure Request
  <*> flag
    (ExistsRenderableFormat . pure <$> (formatHumanTable tz))
    (ExistsRenderableFormat . pure <$> formatJsv)
    (long "jsv")
  <*> option
    (fmap pure $ PushbulletKey <$> raw)
    (long "key" <> value access)
  <*> subparser (
    command "sms" (
      fullDescInfo $ subparser (
        command "list" (
          fullDescInfo $
            pure (\d r -> do
              d' <- maybe device pure d
              pure $ inject <$> do
                case r of
                  Left t -> listSms d' t
                  Right n@(FuzzyName n') -> do
                    fuzzyLookupThread n d' >>= \case
                      Nothing -> commandError $
                        "failed to find recipient with name " <> n'
                      Just t -> listSms d' (t^.threadId)
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> (
              option (Left . SmsThreadId <$> raw) (long "thread")
              <|>
              option (Right . fuzz <$> raw) (long "name")
            )
        )
        <>
        command "send" (
          fullDescInfo $
            pure (\d dest m -> do
              d' <- maybe device pure d
              pure $ inject <$> smartSend dest m d'
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> (
              option (ByNumber . PhoneNumber <$> raw) (long "number")
              <|>
              option (ByName . fuzz <$> raw) (long "to")
            )
            <*> option raw (long "message")
        )
        <>
        command "threads" (
          fullDescInfo $
            pure (\d n -> do
              d' <- maybe device pure d
              pure $ inject <$> do
                threads <- listThreads d'
                let true = const True
                let match = maybe true fuzzyMatchThreadRecipientName n
                pure (filter match threads)
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> optional
              (option (fuzz <$> raw) (long "involving"))
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
        <>
        command "create" (
          fullDescInfo $
            pure (\name sms manuf model icon -> do
              let sms' = maybe NoSms (bool NoSms HasSms) sms
              pure $ inject <$> makeDevice Device
                { _deviceId = ()
                , _deviceActive = ()
                , _deviceCreated = ()
                , _deviceModified = ()
                , _deviceIcon = fromMaybe deviceIconSystem icon
                , _deviceNickname = Just name
                , _deviceGeneratedNickname = ()
                , _deviceManufacturer = manuf
                , _deviceModel = model
                , _deviceAppVersion = Nothing
                , _deviceFingerprint = ()
                , _deviceKeyFingerprint = ()
                , _deviceHasSms = sms'
                , _devicePushToken = Nothing
                }
            )
            <*> option (Nickname <$> raw) (long "name")
            <*> optional (switch (long "has-sms"))
            <*> optional (option (Manufacturer <$> raw) (long "manufacturer"))
            <*> optional (option (Model <$> raw) (long "model"))
            <*> optional (option (DeviceIcon <$> raw) (long "icon"))
        )
        <>
        command "delete" (
          fullDescInfo $
            pure (\did -> pure $ inject <$> removeDevice did)
            <*> option (DeviceId <$> raw) (long "by-id")
        )
      )
    )
    <>
    command "push" (
      fullDescInfo $ subparser (
        command "list" (
          fullDescInfo $
            pure (\c a -> do
              let c' = maybe All id c
              pure $ inject <$> listPushes a c'
            )
            <*> optional (option (Limit <$> auto) (long "limit"))
            <*> switch (long "active")
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
  let env = ClientEnv { manager, baseUrl = url, cookieJar = Nothing }

  let comm = httpCommand key cmd'
  let runClient = flip runClientM env
  let runE = runExceptT
  response <- fmap (liftE2 ServantError CommandError) . runClient . runE $ comm

  case response of
    Left e -> do
      ePutStrLn (show e)
      exitFailure
    Right r -> case match' format r of
      ExistsRenderableFormat f -> printFormat =<< f

-- | Interprets a computation in the 'Command' monad into a computation in the
-- 'ClientM' monad that actually performs HTTP requests.
httpCommand :: PushbulletKey -> Command a -> ExceptT T.Text ClientM a
httpCommand key = iterM phi where
  auth = pushbulletAuth key

  phi :: CommandF (ExceptT T.Text ClientM a) -> ExceptT T.Text ClientM a
  phi com = case com of
    ListSms d t k ->
      k . unSmsMessages
        =<< lift (getSmsMessages auth (d `MessagesIn` t))

    ListPushes b count k -> do
      let f = fmap (fmap unExistingPushes)
      let getPushes' = f . lift . getPushes auth Nothing (Just b) Nothing
      k =<< getPaginatedLimit' count getPushes'

    ListThreads d k ->
      k . unSmsThreads
        =<< lift (getSmsThreads auth (ThreadsOf d))

    SendSms u d n m k ->
      lift (createEphemeral auth (PushEphemeral Nothing (Sms u d n m))) *> k

    ListDevices count k -> do
      let f = fmap (fmap unExistingDevices)
      let getDevices' = f . lift . getDevices auth Nothing
      k =<< getPaginatedLimit' count getDevices'

    MakeDevice d k -> k =<< lift (createDevice auth d)

    RemoveDevice did k -> lift (deleteDevice auth did) *> k

    Me k -> k =<< lift (getMe auth)

    ThrowCommandError e -> throwError e

fullDescInfo :: Parser a -> ParserInfo a
fullDescInfo p = info (helper <*> p) fullDesc

raw :: ReadM T.Text
raw = T.pack <$> str

line :: FilePath -> IO T.Text
line p = decodeUtf8 . BS.init <$> readFile p

device :: IO DeviceId
device = DeviceId . T.pack <$> getEnv "PUSHBULLET_DEVICE"

access :: IO PushbulletKey
access = PushbulletKey . T.pack <$> getEnv "PUSHBULLET_KEY"

type Request'
  = Request IO PushbulletKey

data Error
  = ServantError ServantError
  | CommandError T.Text
  deriving (Eq, Show)

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

-- | Match a name to an sms thread. If there is no name given, always produces
-- True. Otherwise, returns True if and only if there exists a recipient of the
-- thread whose casefolded name contains the given name as a substring.
fuzzyMatchThreadRecipientName :: FuzzyName -> SmsThread a -> Bool
fuzzyMatchThreadRecipientName f t = any (fuzzyMatchName f) names where
  names = (t^.threadRecipients.to toList)^..each.recipientName

fuzzyMatchName :: FuzzyName -> Name -> Bool
fuzzyMatchName (FuzzyName f) (Name n) = f `T.isInfixOf` T.toCaseFold n

data SendDest
  = ByName FuzzyName
  | ByNumber PhoneNumber

newtype FuzzyName = FuzzyName T.Text

fuzz :: T.Text -> FuzzyName
fuzz = FuzzyName . T.toCaseFold

-- | Look up a thread by fuzzy-matching a recipient's name.
fuzzyLookupThread
  :: FuzzyName -> DeviceId -> Command (Maybe (SmsThread'))
fuzzyLookupThread f d = do
  threads <- listThreads d
  pure $ case filter (fuzzyMatchThreadRecipientName f) threads of
    (t:_) -> Just t
    _ -> Nothing

smartSend :: SendDest -> T.Text -> DeviceId -> Command ()
smartSend dest m d = do
  case dest of
    ByName name@(FuzzyName name') -> do
      fuzzyLookupThread name d >>= \case
        Nothing -> commandError $
          "failed to find recipient with name " <> name'
        Just t -> send' (t^.threadRecipients.to N.head.recipientNumber)
    ByNumber n -> send' n
  where
    send' n = do
      u <- me <&> (^. userId)
      sendSms u d n m

liftE2
  :: (a -> e)
  -> (b -> e)
  -> Either a (Either b r)
  -> Either e r
liftE2 f g = either (Left . f) (either (Left . g) Right)
