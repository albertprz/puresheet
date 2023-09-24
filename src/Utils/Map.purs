module App.Utils.Map where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

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

alterJust :: forall k v. Ord k => (Maybe v -> v) -> k -> Map k v -> Map k v
alterJust fn = Map.alter (Just <<< fn)

updateJust :: forall k v. Ord k => (v -> v) -> k -> Map k v -> Map k v
updateJust fn = Map.update (Just <<< fn)
