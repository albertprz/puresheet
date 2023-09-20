module App.Utils.Array where

import Prelude

import App.Utils.Number (dec, inc)
import Data.Array as Array
import Data.Array.NonEmpty (findIndex, head, last, length, mapMaybe, toArray, updateAtIndices, (!!))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Char (fromCharCode, toCharCode)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))

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

findIndex' :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex' = Array.findIndex

findLastIndex' :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex' = Array.findLastIndex

slice' :: forall a. Int -> Int -> Array a -> Array a
slice' = Array.slice

splitAt' :: forall a. Int -> Array a -> { after :: Array a, before :: Array a }
splitAt' = Array.splitAt

sliceNext' :: forall a. Int -> Int -> Array a -> Array a
sliceNext' n idx = slice' idx (idx + n)

updateAt' :: forall a. Int -> a -> Array a -> Maybe (Array a)
updateAt' = Array.updateAt

deleteAt' :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt' = Array.deleteAt

insertAt' :: forall a. Int -> a -> Array a -> Maybe (Array a)
insertAt' = Array.insertAt

zip' :: forall a b. Array a -> Array b -> Array (Tuple a b)
zip' = Array.zip

zipWith' :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith' = Array.zipWith

lookupArray :: forall k v. Ord k => Array k -> Map k v -> Maybe (Array (k /\ v))
lookupArray keys dict = zip' keys <$> vals
  where
  vals = traverse (_ `Map.lookup` dict) keys

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

arr2 :: forall a. a -> a -> Array a
arr2 a b = [ a, b ]

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray (Just x) = [ x ]
maybeToArray Nothing = []

inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange x y value = between x y value ||
  between y x value

distance :: forall a. Range a => a -> a -> Int
distance x y = length $ x .. y

infixr 8 range as ..

class Range a where
  range :: a -> a -> NonEmptyArray a

instance Range Char where
  range c1 c2 = NonEmptyArray $ mapMaybe fromCharCode $ range (toCharCode c1)
    (toCharCode c2)

instance Range Int where
  range = NonEmptyArray.range
