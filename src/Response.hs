{-# LANGUAGE DataKinds #-}

module Response where

import Network.Pushbullet.Types

data Response formatter
  = Response formatter ResponseInfo

data ResponseInfo
  = SmsList [SmsMessage]
  | ThreadList [SmsThread]
  | DeviceList [Device 'Existing]
  | Ok
