module App.Utils.NumberUtils where

import Prelude

import Data.Maybe (Maybe, fromMaybe)

inc :: Int -> Int
inc = (_ + 1)

dec :: Int -> Int
dec = (_ - 1)

pos :: Number -> Boolean
pos = (_ > 0.0)

neg :: Number -> Boolean
neg = (_ < 0.0)

coalesce :: Maybe Number -> Number
coalesce = fromMaybe 0.0
