module App.Utils.Foldable where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable as SemiFoldable

findMapEither
  :: forall a b e f
   . Eq e
  => Foldable f
  => e
  -> (a -> Either e b)
  -> f a
  -> Either e b
findMapEither err mapFn = foldl go (Left err)
  where
  go (Left left) x | left == err = mapFn x
  go x _ = x

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
