module App.Utils.ArrayUtils where

import Prelude

import App.Utils.NumberUtils (dec, inc)
import Data.Array.NonEmpty (findIndex, head, last, mapMaybe, toArray, updateAtIndices, (!!))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple.Nested ((/\))

last' :: forall a. Array a -> Maybe a
last' = Array.last

head' :: forall a. Array a -> Maybe a
head' = Array.head

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

infixr 8 range as ..

class Range a where
  range :: a -> a -> NonEmptyArray a

instance Range Char where
  range c1 c2 = NonEmptyArray $ mapMaybe fromCharCode $ range (toCharCode c1) (toCharCode c2)

instance Range Int where
  range = NonEmptyArray.range

