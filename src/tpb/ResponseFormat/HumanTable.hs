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
import Data.Time.LocalTime ( utcToLocalTime, TimeZone )
import Lens.Micro
import qualified Text.PrettyPrint.ANSI.Leijen as P

newtype HumanTable = HumanTable P.Doc
  deriving RenderableFormat

formatHumanTable
  :: TimeZone -> Product
    '[[SmsMessage], [SmsThread], (), [Device 'Existing], Device 'Existing, [Push 'Existing]]
    HumanTable
formatHumanTable tz
  = smsMessage -| smsThreads -| ok -| devices -| device1 -| pushes -| Inexhaustive where

    chronologicalBy l = sortBy (comparing l)

    pushes :: [Push 'Existing] -> HumanTable
    pushes = HumanTable . P.vcat . map f where
      f :: Push 'Existing -> P.Doc
      f Push{..} = hd P.<$> P.indent 2 body P.<$> foot P.<$> "" where
        hd = pushTypeName P.<+> "from" P.<+> pushSenderType P.<+> senderName

        foot :: P.Doc
        foot = P.text (niceTime tz pushModified)

        body :: P.Doc
        body =
          maybe P.empty id ((("#" P.<+>) . text) <$> pushTitle pushData)
          P.<$>
          case pushData of
            NotePush{..} -> paragraph pushBody
            LinkPush{..} ->
              text (unUrl pushUrl)
              P.<$>
              maybe P.empty paragraph pushLinkBody
            FilePush{..} ->
              text pushFileName P.<+> text (unUrl pushFileUrl)
              P.<$>
              maybe P.empty paragraph pushFileBody

        senderName = text (unName $ pushSenderName pushSender)
        pushTypeName = case pushData of
          NotePush{} -> "Note"
          LinkPush{} -> "Link"
          FilePush{} -> "File"
        pushSenderType = case pushSender of
          SentByUser{} -> "user"
          SentByChannel{} -> "channel"

    smsMessage :: [SmsMessage] -> HumanTable
    smsMessage
      (chronologicalBy (^.smsTime) -> groupSms tenMins -> groups) =
        HumanTable (P.vcat (map phi groups)) where
          phi :: SmsGroup -> P.Doc
          phi (SmsGroup msgs) =
            P.vsep (
              P.hang 2
              . (arrow dir P.<+>)
              . P.fillSep
              . map P.text . words . T.unpack
              . (^.smsBody)
              <$> N.toList msgs
            )
            P.<$> "+" P.<+> P.text t P.<$> "" where
              msg = N.head msgs
              dir = msg^.smsDirection
              t = niceTime tz (msg^.smsTime)

    tenMins :: NominalDiffTime
    tenMins = 60 * 10 -- ten minutes

    smsThreads :: [SmsThread] -> HumanTable
    smsThreads
      = (HumanTable . P.vsep) -- join all the lines
      . map phi -- convert the thread to a line of text
      . chronologicalBy (^.threadLatest.smsTime)
        -- order by latest message in thread
      where
        phi :: SmsThread -> P.Doc
        phi t =
          dirName (msg^.smsDirection) P.<+> rs P.</>
          "on" P.<+> P.text time P.<$>
          P.indent 2 body P.<$>
          "" where
            unpackName = text . unName
            name = unpackName . (^.recipientName)
            recipientNames = map name . N.toList $ t^.threadRecipients
            rs = P.hcat (P.punctuate ", " recipientNames)
            niceBody = P.fillSep . map P.text . words . T.unpack
            msg = t^.threadLatest
            body = niceBody $ msg^.smsBody
            time = niceTime tz (msg^.smsTime)

    ok :: () -> HumanTable
    ok _ = HumanTable $ ""

    devices :: [Device 'Existing] -> HumanTable
    devices = error "nice devices listing is not implemented"

    device1 :: Device 'Existing -> HumanTable
    device1 = error "nice device listing is not implemented"

    arrow dir = case dir of
      OutgoingSms -> ">"
      IncomingSms -> "<"

    dirName dir = case dir of
      OutgoingSms -> "to"
      IncomingSms -> "from"

paragraph :: T.Text -> P.Doc
paragraph = P.fillSep . map P.text . words . T.unpack

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

text :: T.Text -> P.Doc
text = P.text . T.unpack

niceTime :: TimeZone -> PushbulletTime -> String
niceTime tz (PushbulletTime t) =
  formatTime defaultTimeLocale "%a %d %b %Y @ %H:%M:%S" (utcToLocalTime tz t)
