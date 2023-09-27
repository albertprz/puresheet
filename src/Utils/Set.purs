module App.Utils.Set where

import Data.Ord (class Ord)
import Data.Set (Set)
import Data.Set as Set

fromUnfoldable :: forall a. Ord a => Array a -> Set a
fromUnfoldable = Set.fromFoldable
