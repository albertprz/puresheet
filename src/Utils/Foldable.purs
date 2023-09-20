module App.Utils.Foldable where

import App.Utils.Array

import Control.Applicative ((<$>))
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (class Foldable, elem, foldl, length)
import Data.Ring ((-))
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
