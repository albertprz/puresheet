module App.Utils.NumberUtils where

import Prelude

import Data.Maybe (Maybe, fromMaybe)

inc :: forall a. Semiring a => a -> a
inc = (_ + one)

dec :: forall a. Ring a => a -> a
dec = (_ - one)

pos :: forall a. Ord a => Semiring a => a -> Boolean
pos = (_ > zero)

neg :: forall a. Ord a => Semiring a => a -> Boolean
neg = (_ < zero)

abs :: forall a. Ring a => Ord a => a -> a
abs x
  | pos x = x
  | otherwise = negate x

coalesce :: forall a. Semiring a => Maybe a -> a
coalesce = fromMaybe zero
