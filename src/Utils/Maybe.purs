module App.Utils.Maybe where

import Prelude

import Bookhound.FatPrelude (fromJust)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

toMaybe :: forall a. (a -> Boolean) -> a -> Maybe a
toMaybe pred x = if pred x then Just x else Nothing

toMaybe' :: forall a. Boolean -> a -> Maybe a
toMaybe' cond x = if cond then Just x else Nothing

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust x = unsafePartial $ fromJust x
