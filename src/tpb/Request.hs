module Request where

import Command
import Format
import Sum

import Network.Pushbullet.Types

import qualified Data.Text as T

-- | A request is a program for computing the given response combined with a
-- pushbullet api key that can be used to actually execute the program. The
-- command must compute a result in an open sum type that can be handled by a
-- 'Product' to convert that result into a renderable format, possibly using
-- monadic effects.
data Request m env where
  Request
    :: Product ts (ExistsRenderableFormat m)
    -> env
    -> m (Command (Sum' ts))
    -> Request m env

deriving instance Functor (Request m)
deriving instance Foldable (Request m)
deriving instance Traversable (Request m)

data RequestInfo dev count
  = ListSmsReq dev SmsThreadId
  | ListThreadsReq dev
  | SendSmsReq dev PhoneNumber T.Text
  | ListDevicesReq count
