module App.Utils.Foldable where

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)

findMapEither
  :: forall a b e f. Foldable f => e -> (a -> Either e b) -> f a -> Either e b
findMapEither err mapFn = foldl go (Left err)
  where
  go (Left _) x = mapFn x
  go right _ = right
