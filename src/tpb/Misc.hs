module Misc where

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
