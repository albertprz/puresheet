module App.Utils.Bounded where

import Prelude

import App.Utils.Maybe (whenMaybe')
import App.Utils.Number (inc)
import Data.Enum (class BoundedEnum, class Enum, Cardinality, enumFromTo)
import Data.FastVect.MinLenVect (MinLenVect, fromUnsizedNonEmptyArray)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import PointFree ((<..))

infixr 8 enumFromToVect as ..

enumValues :: forall a. BoundedEnum a => MinLenVect 1 a
enumValues = enumFromToVect bottom top

enumFromToVect :: forall a. Enum a => a -> a -> MinLenVect 1 a
enumFromToVect = fromUnsizedNonEmptyArray <.. enumFromTo

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
