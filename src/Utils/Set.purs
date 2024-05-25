module App.Utils.Set where

import Prelude

import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet

fromArray :: forall a. Ord a => Array a -> Set a
fromArray = Set.fromFoldable

nonEmptyIntersection
  :: forall a. Ord a => Set a -> NonEmptySet a -> Maybe (NonEmptySet a)
nonEmptyIntersection x y =
  NonEmptySet.intersection y =<< NonEmptySet.fromSet x
