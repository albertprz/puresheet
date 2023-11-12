module App.Utils.Bounded where

import Prelude

import App.Utils.Maybe (whenMaybe')
import App.Utils.Number (inc)
import Bookhound.FatPrelude (class Newtype)
import Data.Enum (class BoundedEnum, Cardinality, enumFromTo)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Unfoldable1 (class Unfoldable1)

infixr 8 enumFromTo as ..

enumValues :: forall a f. BoundedEnum a => Unfoldable1 f => f a
enumValues = enumFromTo bottom top

getInBoundedRange :: forall a. Bounded a => a -> Maybe a
getInBoundedRange = whenMaybe' inBoundedRange

inBoundedRange :: forall a. Bounded a => a -> Boolean
inBoundedRange = inRange bottom top

clampBounded :: forall a. Ord a => Bounded a => a -> a
clampBounded = clamp bottom top

inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange lo hi value = between lo hi value ||
  between hi lo value

newtypeCardinality :: forall a. Newtype a Int => Bounded a => Cardinality a
newtypeCardinality =
  wrap $ inc (newtypeFromEnum (top @a) - newtypeFromEnum (bottom @a))

newtypeFromEnum :: forall a. Newtype a Int => Bounded a => a -> Int
newtypeFromEnum = (_ - unwrap (bottom @a)) <<< unwrap

newtypeToEnum :: forall a. Newtype a Int => Bounded a => Int -> Maybe a
newtypeToEnum = whenMaybe' inBoundedRange <<< wrap <<< (_ + unwrap (bottom @a))
