module App.Utils.Foldable where

import App.Utils.Array

import Control.Applicative ((<$>))
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (class Foldable, elem, foldl, length)
import Data.Ord (class Ord, Ordering)
import Data.Ring ((-))
import Data.Semigroup (class Semigroup)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable as SemiFoldable
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))

findMapEither
  :: forall a b e f. Foldable f => e -> (a -> Either e b) -> f a -> Either e b
findMapEither err mapFn = foldl go (Left err)
  where
  go (Left _) x = mapFn x
  go right _ = right

filterByIndexes :: forall a f. Foldable f => f Int -> Array a -> Array a
filterByIndexes idxs arr = fst <$>
  filter (\(_ /\ idx) -> idx `elem` idxs)
    (arr `zip'` toArray (0 .. (length arr - 1)))

maximum1 :: forall f a. Ord a => Foldable1 f => f a -> a
maximum1 = SemiFoldable.maximum

maximumBy1 :: forall f a. Foldable1 f => (a -> a -> Ordering) -> f a -> a
maximumBy1 = SemiFoldable.maximumBy

minimum1 :: forall f a. Ord a => Foldable1 f => f a -> a
minimum1 = SemiFoldable.minimum

minimumBy1 :: forall f a. Foldable1 f => (a -> a -> Ordering) -> f a -> a
minimumBy1 = SemiFoldable.minimumBy

intercalate1 :: forall f m. Foldable1 f => Semigroup m => m -> f m -> m
intercalate1 = SemiFoldable.intercalate
