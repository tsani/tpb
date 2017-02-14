{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Sum where

import Data.Kind
import Data.Proxy
import GHC.Exts ( Constraint )
import GHC.TypeLits

type family Demote' (p :: KProxy k) :: k -> Type

type Demote (a :: k) = Demote' ('KProxy :: KProxy k)

class Reflect (a :: k) where
  reflect :: proxy a -> Demote' ('KProxy :: KProxy k) a

data SumStatus
  = EmptySum
  | NonEmptySum

data Sum :: SumStatus -> [Type] -> Type where
  Case :: x -> Sum 'NonEmptySum (x ': xs)
  Skip :: Sum 'NonEmptySum xs -> Sum 'NonEmptySum (x ': xs)

type Sum' = Sum 'NonEmptySum

data Product :: [Type] -> Type -> Type where
  Inexhaustive :: Product '[] a
  Match :: (a -> b) -> Product as b -> Product (a ': as) b

deriving instance Functor (Product ts)

(-|) :: (a -> b) -> Product as b -> Product (a ': as) b
(-|) = Match
infixr 6 -|

data Elem :: k -> [k] -> Type where
  Here :: Elem a (a ': as)
  There :: Elem a as -> Elem a (a' ': as)

class Inject (t :: Type) (ts :: [Type]) where
  inject :: t -> Sum 'NonEmptySum ts

instance
  TypeError (
    'Text "cannot inject " ':<>:
    'ShowType t ':<>:
    'Text " into empty list."
  )
  => Inject t '[] where
  inject = error "inject is impossible"

instance {-# OVERLAPPABLE #-} Inject s ts => Inject s (t ': ts) where
  inject t = Skip (inject t)

instance {-# OVERLAPPING #-} Inject t (t ': ts) where
  inject t = Case t

-- | We can inject into open sums if we can prove that the value we want to
-- inject is in the sum.
inject' :: t -> Elem t ts -> Sum 'NonEmptySum ts
inject' x Here = Case x
inject' x (There e) = Skip (inject' x e)

-- | Pattern matching.
--
-- Given an element in a disjoint union of types and a handler for each of
-- those types to convert it to some output type, lookup the corresponding
-- handler in the union and run it.
match' :: Product ts t -> Sum 'NonEmptySum ts -> t
match' (Match _ p) (Skip s) = match' p s
match' (Match f _) (Case x) = f x
-- actually these cases are exhaustive.

type family ElemC (x :: k) (xs :: [k]) :: Constraint where
  ElemC x '[]
    = TypeError (
      'Text "Type " ':<>:
      'ShowType x ':<>:
      'Text " was not found in the list"
    )
  ElemC x (x ': xs) = ()
  ElemC x (y ': xs) = ElemC x xs
