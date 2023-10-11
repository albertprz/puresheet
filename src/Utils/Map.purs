module App.Utils.Map where

import Prelude

import App.Utils.Array (zip')
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))

lookupArray :: forall k v. Ord k => Array k -> Map k v -> Maybe (Array (k /\ v))
lookupArray keys dict = zip' keys <$> vals
  where
  vals = traverse (_ `Map.lookup` dict) keys

lookup2
  :: forall k1 k2 v. Ord k1 => Ord k2 => k1 -> Map k2 v -> Map k1 k2 -> Maybe v
lookup2 k dict1 dict2 = (_ `Map.lookup` dict1) =<< Map.lookup k dict2

swapKey :: forall k v. Ord k => k /\ k -> Map k v -> Map k v
swapKey (k1 /\ k2) dict =
  Map.alter (const v2) k1 $
    Map.alter (const v1) k2 dict
  where
  v1 = Map.lookup k1 dict
  v2 = Map.lookup k2 dict

deleteWhen :: forall k v. Ord k => (k -> Boolean) -> Map k v -> Map k v
deleteWhen f dict =
  foldl (flip Map.delete) dict
    $ Set.filter f
    $ Map.keys dict

alterJust :: forall k v. Ord k => (Maybe v -> v) -> k -> Map k v -> Map k v
alterJust fn = Map.alter (Just <<< fn)

updateJust :: forall k v. Ord k => (v -> v) -> k -> Map k v -> Map k v
updateJust fn = Map.update (Just <<< fn)
