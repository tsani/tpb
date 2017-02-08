{-|
 - This module describes a free monad for some high-level operations on the
 - Pushbullet API.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Command where

import Network.Pushbullet.Types
import Network.Pushbullet.Misc ( Count )

import Control.Monad.Free
import qualified Data.Text as T

--  | The base functor for the 'Command' free monad.
data CommandF a where
  -- | List the SMS messages in a thread on a device.
  ListSms :: DeviceId -> SmsThreadId -> ([SmsMessage] -> a) -> CommandF a
  -- | List the SMS threads on a device.
  ListThreads :: DeviceId -> ([SmsThread] -> a) -> CommandF a
  -- | Send an SMS with a device to a phone.
  SendSms :: UserId -> DeviceId -> PhoneNumber -> T.Text -> a -> CommandF a
  -- | List the devices.
  ListDevices :: Count -> ([Device 'Existing] -> a) -> CommandF a
  Me :: (User -> a) -> CommandF a
  deriving Functor

-- | The Command monad.
type Command = Free CommandF

listSms :: DeviceId -> SmsThreadId -> Command [SmsMessage]
listSms d t = liftF (ListSms d t id)

listThreads :: DeviceId -> Command [SmsThread]
listThreads d = liftF (ListThreads d id)

sendSms :: UserId -> DeviceId -> PhoneNumber -> T.Text -> Command ()
sendSms u d p t = liftF (SendSms u d p t ())

listDevices :: Count -> Command [Device 'Existing]
listDevices c = liftF (ListDevices c id)

me :: Command User
me = liftF (Me id)
