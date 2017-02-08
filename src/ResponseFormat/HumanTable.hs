{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ResponseFormat.HumanTable where

import Format
import Sum

import Network.Pushbullet.Types

import Data.Function ( on )
import Data.List ( sortBy )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Clock ( diffUTCTime, NominalDiffTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Text.PrettyPrint.ANSI.Leijen as P

newtype HumanTable = HumanTable P.Doc
  deriving RenderableFormat

formatHumanTable
  :: Product
    '[[SmsMessage], [SmsThread], (), [Device 'Existing]]
    (IO HumanTable)
formatHumanTable
  = smsMessage -| smsThreads -| ok -| devices -| Inexhaustive where

    chronologicalBy f = sortBy (comparing (smsTime . f))

    smsMessage :: [SmsMessage] -> IO HumanTable
    smsMessage (chronologicalBy id -> groupSms tenMins -> groups) =
      HumanTable . P.vcat <$> (mapM phi groups) where
        phi :: SmsGroup -> IO P.Doc
        phi (SmsGroup msgs) = do
          let msg = N.head msgs
          let dir = smsDirection msg
          t <- niceTime (smsTime msg)
          pure $
            P.vsep (
              P.hang 2
              . (arrow dir P.<+>)
              . P.fillSep
              . map P.text . words . T.unpack
              . smsBody
              <$> N.toList msgs
            )
            P.<$> "+" P.<+> P.text t P.<$> ""

    tenMins :: NominalDiffTime
    tenMins = 60 * 10 -- ten minutes

    smsThreads :: [SmsThread] -> IO HumanTable
    smsThreads
      = fmap (HumanTable . P.vsep) -- join all the lines
      . mapM phi -- convert the thread to a line of text
      . chronologicalBy threadLatest -- order by latest message in thread
      where
        phi :: SmsThread -> IO P.Doc
        phi SmsThread{..} = do
          -- let SmsThreadId i = threadId
          let SmsMessage{..} = threadLatest
          let name = P.text . T.unpack. unName . recipientName
          let recipientNames = map name . N.toList $ threadRecipients
          let rs = P.hcat (P.punctuate ", " recipientNames)
          let body = P.fillSep . map P.text . words . T.unpack $ smsBody
          t <- niceTime smsTime
          pure $
            dirName smsDirection P.<+> rs P.</>
            "on" P.<+> P.text t P.<$>
            P.indent 2 body P.<$>
            ""

    ok :: () -> IO HumanTable
    ok _ = pure . HumanTable $ ""

    devices :: [Device 'Existing] -> IO HumanTable
    devices = error "nice devices listing is not implemented"

    niceTime (PushbulletTime t) =
      formatTime defaultTimeLocale "%a %d %b %Y @ %H:%M:%S"
        <$> (utcToLocalTime <$> getTimeZone t <*> pure t)

    arrow dir = case dir of
      OutgoingSms -> ">"
      IncomingSms -> "<"

    dirName dir = case dir of
      OutgoingSms -> "to"
      IncomingSms -> "from"

-- | An SMS group is a bunch of messages from a given person ordered
-- chronologically such that the timestamps of adjacent messages differ by no
-- more than a fixed amount.
newtype SmsGroup
  = SmsGroup
    { groupMessages :: NonEmpty SmsMessage
    }

smsGroupDir :: SmsGroup -> SmsDirection
smsGroupDir = smsDirection . N.head . groupMessages

-- | Given a list of SMS ordered chronologically and a maximum time difference,
-- group the messages.
groupSms :: NominalDiffTime -> [SmsMessage] -> [SmsGroup]
groupSms d
  = concat -- so we have to flatten the groups of groups to get just groups
  . fmap (N.toList . fmap SmsGroup . timeframe)
    -- within each group, group by time (no more than 10 minutes apart)
    -- this gives s *group of groups*
  . samedir -- group by direction
  where
    timeframe :: NonEmpty SmsMessage -> NonEmpty (NonEmpty SmsMessage)
    timeframe = N.groupBy1 checkTime

    samedir :: [SmsMessage] -> [NonEmpty SmsMessage]
    samedir = N.groupBy ((==) `on` smsDirection)

    checkTime
      (SmsMessage{smsTime=PushbulletTime u1})
      (SmsMessage{smsTime=PushbulletTime u2}) = diffUTCTime u2 u1 < d
