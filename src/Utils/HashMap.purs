module App.Utils.HashMap where

import Prelude

import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

lookup2
  :: forall k1 k2 v
   . Hashable k1
  => Hashable k2
  => k1
  -> HashMap k1 k2
  -> HashMap k2 v
  -> Maybe v
lookup2 k dict1 dict2 = (flip HashMap.lookup dict2) =<< HashMap.lookup k dict1

swapKey :: forall k v. Hashable k => k /\ k -> HashMap k v -> HashMap k v
swapKey (k1 /\ k2) dict =
  HashMap.alter (const v2) k1 $
    HashMap.alter (const v1) k2 dict
  where
  v1 = HashMap.lookup k1 dict
  v2 = HashMap.lookup k2 dict

swapKeys
  :: forall f k v
   . Foldable f
  => Hashable k
  => f (k /\ k)
  -> HashMap k v
  -> HashMap k v
swapKeys = flip $ foldl $ flip swapKey

deleteWhen
  :: forall k v. Hashable k => (k -> Boolean) -> HashMap k v -> HashMap k v
deleteWhen f dict =
  flip bulkDelete dict
    $ filter f
    $ HashMap.keys dict

bulkDelete
  :: forall f k v. Foldable f => Hashable k => f k -> HashMap k v -> HashMap k v
bulkDelete = flip $ foldl $ flip HashMap.delete

alterJust
  :: forall k v. Hashable k => (Maybe v -> v) -> k -> HashMap k v -> HashMap k v
alterJust fn = HashMap.alter (Just <<< fn)

updateJust
  :: forall k v. Hashable k => (v -> v) -> k -> HashMap k v -> HashMap k v
updateJust fn = HashMap.update (Just <<< fn)
