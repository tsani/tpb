{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bichan where

import Control.Concurrent.Chan
import Data.Functor.Contravariant

data Bichan a b =
  Bichan { input :: Chan a, output :: Chan b }

newBichan :: IO (Bichan a b)
newBichan = Bichan <$> newChan <*> newChan

biflip :: Bichan a b -> Bichan b a
biflip Bichan{..} = Bichan{input = output, output = input}

send :: Bichan a b -> a -> IO ()
send Bichan{input} x = writeChan input x

recv :: Bichan a b -> IO b
recv Bichan{output} = readChan output

request :: Bichan a b -> a -> IO b
request b x = send b x *> recv b

handle :: Bichan a b -> (b -> IO a) -> IO ()
handle bi f = recv bi >>= f >>= send bi
