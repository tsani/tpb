{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ResponseFormat.JSV
( formatJsv
) where

import Format
import Sum
import Misc

import Network.Pushbullet.Types

import Data.Aeson ( ToJSON(..), encode, (.=), object, Value(Number) )
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )
import Lens.Micro
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
  :: Product
    '[[SmsMessage], [SmsThread], (), [Device 'Existing], Device 'Existing, [Push 'Existing]]
    JSV
formatJsv
  = JSV
  . map pure
  <$> (sms -| threads -| ok -| devices -| device -| pushes -| Inexhaustive) where
    device :: Device 'Existing -> [JsvCell]
    device = pure . JsvCell . Formatted

    sms :: [SmsMessage] -> [JsvCell]
    sms = map (JsvCell . Formatted)

    threads :: [SmsThread] -> [JsvCell]
    threads = map (JsvCell . Formatted)

    devices :: [Device 'Existing] -> [JsvCell]
    devices = map (JsvCell . Formatted)

    pushes :: [Push 'Existing] -> [JsvCell]
    pushes = map (JsvCell . Formatted)

    ok :: () -> [JsvCell]
    ok _ = pure $ JsvCell @T.Text "ok"

-- | A simple newtype wrapper so that we can special ToJSON instances for the
-- output.
newtype Formatted a = Formatted a

instance ToJSON (Formatted PushbulletTime) where
  toJSON (Formatted (PushbulletTime t)) = Number d where
    d = fromRational (toRational $ utcTimeToPOSIXSeconds t)

instance ToJSON (Formatted SmsMessage) where
  toJSON (Formatted msg) = object
    [ "direction" .= id @T.Text (
      case msg^.smsDirection of
        IncomingSms -> "incoming"
        OutgoingSms -> "outgoing"
      )
    , "time" .= Formatted (msg^.smsTime)
    , "body" .= (msg^.smsBody)
    , "smsId" .= (msg^.smsId)
    , "smsType" .= id @T.Text (
      case msg^.smsType of
        SMS -> "sms"
        MMS -> "mms"
      )
    ]

instance ToJSON (Formatted SmsThread) where
  toJSON (Formatted t) = object
    [ "id" .= (t^.threadId)
    , "recipients" .= (Formatted <$> t^.threadRecipients)
    , "latest" .= Formatted (t^.threadLatest)
    ]

instance ToJSON (Formatted SmsThreadRecipient) where
  toJSON (Formatted r) = object
    [ "name" .= (r^.recipientName)
    , "number" .= (r^.recipientNumber)
    ]

instance ToJSON (Formatted (Device 'Existing)) where
  toJSON (Formatted d) = object
    [ "id" .= (d^.deviceId)
    , "active" .= (d^.deviceActive)
    , "name" .= (d^.deviceNickname)
    , "hasSms" .= (d^.deviceHasSms)
    , "manufacturer" .= (d^.deviceManufacturer)
    , "model" .= (d^.deviceModel)
    ]

instance ToJSON (Formatted (Push 'Existing)) where
  toJSON (Formatted Push{..}) = object
    [ "id" .= pushId
    , "active" .= pushActive
    , "created" .= pushCreated
    , "modified" .= pushModified
    , "dismissed" .= pushDismissed
    , "direction" .= pushDirection
    , "sender" .= case pushSender of
      SentByUser {..} -> object
        [ "type" .= id @String "user"
        , "id" .= pushSenderUserId
        , "clientId" .= pushSenderClientId
        , "email" .= pushSenderUserEmail
        , "emailNormalized" .= pushSenderUserEmailNormalized
        , "name" .= pushSenderName
        ]
      SentByChannel {..} -> object
        [ "type" .= id @String "channel"
        , "id" .= pushSenderChannelId
        , "name" .= pushSenderName
        ]
    , "receiver" .= pushReceiver <#> \case
      ReceivedByUser {..} -> object
        [ "type" .= id @String "user"
        , "id" .= pushReceiverUserId
        , "email" .= pushReceiverEmail
        , "emailNormalized" .= pushReceiverEmailNormalized
        ]
    , "sourceDevice" .= pushSourceDevice
    , "target" .= case pushTarget of
      SentBroadcast -> object
        [ "type" .= id @String "broadcast"
        ]
      SentToDevice d -> object
        [ "type" .= id @String "sentToDevice"
        , "id" .= d
        ]
    , "guid" .= pushGuid
    , "data" .= case pushData of
      NotePush {..} -> object
        [ "title" .= pushTitle
        , "body" .= pushBody
        , "type" .= id @String "note"
        ]
      LinkPush {..} -> object
        [ "title" .= pushTitle
        , "body" .= pushLinkBody
        , "link" .= pushUrl
        , "type" .= id @String "link"
        ]
      FilePush {..} -> object
        [ "title" .= pushTitle
        , "body" .= pushFileBody
        , "filetype" .= pushFileType
        , "url" .= pushFileUrl
        , "imageUrl" .= pushFileUrl
        , "imageWidth" .= pushImageWidth
        , "imageHeight" .= pushImageHeight
        ]
    ]
