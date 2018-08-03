{-|
 - This module describes a free monad for some high-level operations on the
 - Pushbullet API.
 -}

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
  ListThreads :: DeviceId -> ([SmsThread'] -> a) -> CommandF a

  -- | List the pushes sent to a device.
  ListPushes :: Bool -> Count -> ([Push 'Existing] -> a) -> CommandF a

  -- | Send an SMS with a device to a phone.
  SendSms :: UserId -> DeviceId -> PhoneNumber -> T.Text -> a -> CommandF a

  -- | List the devices.
  ListDevices :: Count -> ([Device 'Existing] -> a) -> CommandF a

  -- | Create a new device.
  MakeDevice :: Device 'New -> (Device 'Existing -> a) -> CommandF a

  -- | Remove an existing device.
  RemoveDevice :: DeviceId -> a -> CommandF a

  -- | Get the current user info.
  Me :: (User -> a) -> CommandF a

  -- | Raise an exception.
  ThrowCommandError :: T.Text -> CommandF a

  deriving Functor

-- | The Command monad.
type Command = Free CommandF

listSms :: DeviceId -> SmsThreadId -> Command [SmsMessage]
listSms d t = liftF (ListSms d t id)

listPushes
  :: Bool -- ^ Whether to list only active pushes.
  -> Count -- ^ The number of pushes to list.
  -> Command [Push 'Existing]
listPushes b c = liftF (ListPushes b c id)

removeDevice :: DeviceId -> Command ()
removeDevice d = liftF (RemoveDevice d ())

listThreads :: DeviceId -> Command [SmsThread']
listThreads d = liftF (ListThreads d id)

sendSms :: UserId -> DeviceId -> PhoneNumber -> T.Text -> Command ()
sendSms u d p t = liftF (SendSms u d p t ())

listDevices :: Count -> Command [Device 'Existing]
listDevices c = liftF (ListDevices c id)

makeDevice :: Device 'New -> Command (Device 'Existing)
makeDevice d = liftF (MakeDevice d id)

me :: Command User
me = liftF (Me id)

commandError :: T.Text -> Command a
commandError = Free . ThrowCommandError
