module App.Utils.MinLenVect (fromFoldable, sort, zip, zipGT, zipLT) where

import Prelude

import Data.Array as Array
import Data.FastVect.MinLenVect (MinLenVect, fromUnsizedArray, toArray)
import Data.Foldable (class Foldable)
import Data.Tuple.Nested (type (/\))
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Unsafe.Coerce (unsafeCoerce)

sort
  :: forall len a
   . Ord a
  => Compare len (-1) GT
  => MinLenVect len a
  -> MinLenVect len a
sort = overArray Array.sort

zipGT
  :: forall len len' a b
   . Compare len (-1) GT
  => Compare len' (-1) GT
  => Compare len' len GT
  => MinLenVect len a
  -> MinLenVect len' b
  -> MinLenVect len (a /\ b)
zipGT xs ys = unsafeFromArray $ Array.zip (toArray xs) (toArray ys)

zipLT
  :: forall len len' a b
   . Compare len (-1) GT
  => Compare len' (-1) GT
  => Compare len' len LT
  => MinLenVect len a
  -> MinLenVect len' b
  -> MinLenVect len' (a /\ b)
zipLT xs ys = unsafeFromArray $ Array.zip (toArray xs) (toArray ys)

zip
  :: forall len a b
   . Compare len (-1) GT
  => MinLenVect len a
  -> MinLenVect len b
  -> MinLenVect len (a /\ b)
zip xs ys = unsafeFromArray $ Array.zip (toArray xs) (toArray ys)

fromFoldable :: forall f a. Foldable f => f a -> MinLenVect 0 a
fromFoldable = fromUnsizedArray <<< Array.fromFoldable

overArray
  :: forall len a b
   . Compare len (-1) GT
  => (Array a -> Array b)
  -> MinLenVect len a
  -> MinLenVect len b
overArray f = unsafeFromArray <<< f <<< toArray

unsafeFromArray :: forall len a. Array a -> MinLenVect len a
unsafeFromArray = unsafeCoerce <<< fromUnsizedArray
