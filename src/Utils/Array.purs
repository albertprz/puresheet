module App.Utils.Array where

import Prelude

import App.Utils.Maybe (whenMaybe')
import Data.Array as Array
import Data.Array.NonEmpty (toArray)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple)

getElemSat :: forall a. Bounded a => a -> Maybe a
getElemSat = whenMaybe' (inRange bottom top)

arr2 :: forall a. a -> a -> Array a
arr2 a b = [ a, b ]

inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange x y value = between x y value ||
  between y x value

head' :: forall a. Array a -> Maybe a
head' = Array.head

tail' :: forall a. Array a -> Maybe (Array a)
tail' = Array.tail

last' :: forall a. Array a -> Maybe a
last' = Array.last

init' :: forall a. Array a -> Maybe (Array a)
init' = Array.init

uncons' :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons' = Array.uncons

unsnoc' :: forall a. Array a -> Maybe { init :: Array a, last :: a }
unsnoc' = Array.unsnoc

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

index' :: forall a. Array a -> Int -> Maybe a
index' = Array.index

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

intersperse' :: forall a. a -> Array a -> Array a
intersperse' = Array.intersperse
