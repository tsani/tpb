{-# LANGUAGE OverloadedStrings #-}

module Format where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- | The class of output formats that can be rendered.
class RenderableFormat a where
  renderFormat :: a -> LBS.ByteString

  -- | If the format can be rendered in a special way directly to some output,
  -- then this method can be used. It is implemented by default by simply
  -- writing the format rendered as a bytestring.
  printFormat :: a -> IO ()
  printFormat = LBS.putStr . renderFormat

instance RenderableFormat LBS.ByteString where
  renderFormat = id

instance RenderableFormat BS.ByteString where
  renderFormat = LBS.fromStrict

instance RenderableFormat P.Doc where
  renderFormat
    = LBS.fromStrict
    . encodeUtf8
    . T.pack
    . ($ "")
    . P.displayS
    . P.renderPretty 0.75 80

instance RenderableFormat T.Text where
  renderFormat = LBS.fromStrict . encodeUtf8

-- | Existential that wraps up a renderable format applied to some type
-- constructor.
data ExistsRenderableFormat f where
  ExistsRenderableFormat
    :: RenderableFormat fmt
    => f fmt
    -> ExistsRenderableFormat f

-- | A monad for formatting text. This is just a kleisli arrow!
newtype FormatM i m o = FormatM { unFormatM :: i -> m o }
