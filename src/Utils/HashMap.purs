module App.Utils.HashMap where

import Prelude

import App.Utils.Array (zip')
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))

lookupArray
  :: forall k v. Hashable k => Array k -> HashMap k v -> Maybe (Array (k /\ v))
lookupArray keys dict = zip' keys <$> vals
  where
  vals = traverse (flip HashMap.lookup dict) keys

lookup2
  :: forall k1 k2 v
   . Hashable k1
  => Hashable k2
  => k1
  -> HashMap k2 v
  -> HashMap k1 k2
  -> Maybe v
lookup2 k dict1 dict2 = (flip HashMap.lookup dict1) =<< HashMap.lookup k dict2

swapKey :: forall k v. Hashable k => k /\ k -> HashMap k v -> HashMap k v
swapKey (k1 /\ k2) dict =
  HashMap.alter (const v2) k1 $
    HashMap.alter (const v1) k2 dict
  where
  v1 = HashMap.lookup k1 dict
  v2 = HashMap.lookup k2 dict

deleteWhen
  :: forall k v. Hashable k => (k -> Boolean) -> HashMap k v -> HashMap k v
deleteWhen f dict =
  foldl (flip HashMap.delete) dict
    $ filter f
    $ HashMap.keys dict

alterJust
  :: forall k v. Hashable k => (Maybe v -> v) -> k -> HashMap k v -> HashMap k v
alterJust fn = HashMap.alter (Just <<< fn)

updateJust
  :: forall k v. Hashable k => (v -> v) -> k -> HashMap k v -> HashMap k v
updateJust fn = HashMap.update (Just <<< fn)
