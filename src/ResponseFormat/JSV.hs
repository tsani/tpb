{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ResponseFormat.JSV
( formatJsv
) where

import Format
import Sum

import Network.Pushbullet.Types

import Data.Aeson ( ToJSON(..), encode, (.=), object, Value(Number) )
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )

newtype JSV = JSV [[JsvCell]]

instance RenderableFormat JSV where
  renderFormat (JSV rows)
    = LBS.concat ((<> "\n") . LBS.intercalate "," . fmap encode <$> rows)

data JsvCell where
  JsvCell :: ToJSON a => !a -> JsvCell

instance ToJSON JsvCell where
  toJSON (JsvCell cell) = toJSON cell

formatJsv
  :: Product '[[SmsMessage], [SmsThread], (), [Device 'Existing]] JSV
formatJsv
  = JSV . map pure <$> (sms -| threads -| ok -| devices -| Inexhaustive) where
    sms :: [SmsMessage] -> [JsvCell]
    sms = map (JsvCell . Formatted)

    threads :: [SmsThread] -> [JsvCell]
    threads = map (JsvCell . Formatted)

    devices :: [Device 'Existing] -> [JsvCell]
    devices = map (JsvCell . Formatted)

    ok :: () -> [JsvCell]
    ok _ = pure $ JsvCell @T.Text "ok"

-- | A simple newtype wrapper so that we can special ToJSON instances for the
-- output.
newtype Formatted a = Formatted a

instance ToJSON (Formatted PushbulletTime) where
  toJSON (Formatted (PushbulletTime t)) = Number d where
    d = fromRational (toRational $ utcTimeToPOSIXSeconds t)

instance ToJSON (Formatted SmsMessage) where
  toJSON (Formatted SmsMessage{..}) = object
    [ "direction" .= id @T.Text (
      case smsDirection of
        IncomingSms -> "incoming"
        OutgoingSms -> "outgoing"
      )
    , "time" .= Formatted smsTime
    , "body" .= smsBody
    , "smsId" .= smsId
    , "smsType" .= id @T.Text (
      case smsType of
        SMS -> "sms"
        MMS -> "mms"
      )
    ]

instance ToJSON (Formatted SmsThread) where
  toJSON (Formatted SmsThread{..}) = object
    [ "id" .= threadId
    , "recipients" .= (Formatted <$> threadRecipients)
    , "latest" .= Formatted threadLatest
    ]

instance ToJSON (Formatted SmsThreadRecipient) where
  toJSON (Formatted SmsThreadRecipient{..}) = object
    [ "name" .= recipientName
    , "number" .= recipientNumber
    ]

instance ToJSON (Formatted (Device 'Existing)) where
  toJSON (Formatted Device{..}) = object
    [ "id" .= deviceId
    , "active" .= deviceActive
    , "name" .= deviceNickname
    , "hasSms" .= deviceHasSms
    , "manufacturer" .= deviceManufacturer
    , "model" .= deviceModel
    ]
