module App.Utils.ArrayUtils where

import Prelude

import App.Utils.NumberUtils (dec, inc)
import Data.Array as Array
import Data.Array.NonEmpty (findIndex, head, last, length, mapMaybe, toArray, updateAtIndices, (!!))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple.Nested ((/\))

head' :: forall a. Array a -> Maybe a
head' = Array.head

tail' :: forall a. Array a -> Maybe (Array a)
tail' = Array.tail

last' :: forall a. Array a -> Maybe a
last' = Array.last

init' :: forall a. Array a -> Maybe (Array a)
init' = Array.init

take' :: forall a. Int -> Array a -> Array a
take' = Array.take

takeEnd' :: forall a. Int -> Array a -> Array a
takeEnd' = Array.takeEnd

drop' :: forall a. Int -> Array a -> Array a
drop' = Array.drop

dropEnd' :: forall a. Int -> Array a -> Array a
dropEnd' = Array.dropEnd

toArray' :: forall a. Maybe (NonEmptyArray a) -> Array a
toArray' = maybe [] toArray

getNextElemSat :: forall a. Eq a => NonEmptyArray a -> a -> Maybe a
getNextElemSat = getElemSat inc

getPrevElemSat :: forall a. Eq a => NonEmptyArray a -> a -> Maybe a
getPrevElemSat = getElemSat dec

switchElements :: forall a. Eq a => a -> a -> NonEmptyArray a -> NonEmptyArray a
switchElements x y seq = fromMaybe seq $ do
  idx1 <- findIndex (_ == x) seq
  idx2 <- findIndex (_ == y) seq
  pure $ updateAtIndices [ idx1 /\ y, idx2 /\ x ] seq

getElemSat :: forall a. Eq a => (Int -> Int) -> NonEmptyArray a -> a -> Maybe a
getElemSat f seq value = (seq !!! _) <<< f <$> idx
  where
  idx = findIndex (_ == value) seq

infixl 8 satIndex as !!!

satIndex :: forall a. NonEmptyArray a -> Int -> a
satIndex seq idx = fromMaybe bound (seq !! idx)
  where
  bound
    | idx < 0 = head seq
    | otherwise = last seq

inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange x y value = between x y value ||
  between y x value

distance :: forall a. Range a => a -> a -> Int
distance x y = length $ x .. y

infixr 8 range as ..

class Range a where
  range :: a -> a -> NonEmptyArray a

instance Range Char where
  range c1 c2 = NonEmptyArray $ mapMaybe fromCharCode $ range (toCharCode c1) (toCharCode c2)

instance Range Int where
  range = NonEmptyArray.range
