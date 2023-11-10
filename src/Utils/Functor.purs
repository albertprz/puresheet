module App.Utils.Functor where

import Prelude

import App.Utils.Monoid (whenPlus)
import Data.Filterable (class Filterable, filterMap)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

infixl 8 mapp as <$$>

mapp :: forall a b f g. Functor f => Functor g => (a -> b) -> g (f a) -> g (f b)
mapp f = map (map f)

filterByIndex
  :: forall f i a
   . Filterable f
  => FunctorWithIndex i f
  => (i -> Boolean)
  -> f a
  -> f a
filterByIndex f = filterMap pred <<< mapWithIndex Tuple
  where
  pred (idx /\ val) = whenPlus (f idx) (Just val)

filterByIndexes
  :: forall f a i
   . Filterable f
  => FunctorWithIndex i f
  => Ord i
  => Set i
  -> f a
  -> f a
filterByIndexes idxs = filterByIndex (flip Set.member idxs)
