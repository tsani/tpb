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
import Lens.Micro
import qualified Text.PrettyPrint.ANSI.Leijen as P

newtype HumanTable = HumanTable P.Doc
  deriving RenderableFormat

formatHumanTable
  :: Product
    '[[SmsMessage], [SmsThread], (), [Device 'Existing], Device 'Existing]
    (IO HumanTable)
formatHumanTable
  = smsMessage -| smsThreads -| ok -| devices -| device1 -| Inexhaustive where

    chronologicalBy l = sortBy (comparing l)

    smsMessage :: [SmsMessage] -> IO HumanTable
    smsMessage
      (chronologicalBy (^.smsTime) -> groupSms tenMins -> groups) =
        HumanTable . P.vcat <$> (mapM phi groups) where
          phi :: SmsGroup -> IO P.Doc
          phi (SmsGroup msgs) = do
            let msg = N.head msgs
            let dir = msg^.smsDirection
            t <- niceTime (msg^.smsTime)
            pure $
              P.vsep (
                P.hang 2
                . (arrow dir P.<+>)
                . P.fillSep
                . map P.text . words . T.unpack
                . (^.smsBody)
                <$> N.toList msgs
              )
              P.<$> "+" P.<+> P.text t P.<$> ""

    tenMins :: NominalDiffTime
    tenMins = 60 * 10 -- ten minutes

    smsThreads :: [SmsThread] -> IO HumanTable
    smsThreads
      = fmap (HumanTable . P.vsep) -- join all the lines
      . mapM phi -- convert the thread to a line of text
      . chronologicalBy (^.threadLatest.smsTime)
        -- order by latest message in thread
      where
        phi :: SmsThread -> IO P.Doc
        phi t = do
          -- let SmsThreadId i = threadId
          -- let SmsMessage{..} = threadLatest
          let unpackName = P.text . T.unpack . unName
          let name = unpackName . (^.recipientName)
          let recipientNames = map name . N.toList $ t^.threadRecipients
          let rs = P.hcat (P.punctuate ", " recipientNames)
          let niceBody = P.fillSep . map P.text . words . T.unpack
          let msg = t^.threadLatest
          let body = niceBody $ msg^.smsBody
          time <- niceTime (msg^.smsTime)
          pure $
            dirName (msg^.smsDirection) P.<+> rs P.</>
            "on" P.<+> P.text time P.<$>
            P.indent 2 body P.<$>
            ""

    ok :: () -> IO HumanTable
    ok _ = pure . HumanTable $ ""

    devices :: [Device 'Existing] -> IO HumanTable
    devices = error "nice devices listing is not implemented"

    device1 :: Device 'Existing -> IO HumanTable
    device1 = error "nice device listing is not implemented"

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
smsGroupDir = (^.smsDirection) . N.head . groupMessages

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
    samedir = N.groupBy ((==) `on` (^.smsDirection))

    checkTime
      (SmsMessage{_smsTime=PushbulletTime u1})
      (SmsMessage{_smsTime=PushbulletTime u2}) = diffUTCTime u2 u1 < d
