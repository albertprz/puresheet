module App.Utils.Maybe where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

whenMaybe :: forall a. Boolean -> a -> Maybe a
whenMaybe cond x = if cond then Just x else Nothing

unlessMaybe :: forall a. Boolean -> a -> Maybe a
unlessMaybe = whenMaybe <<< not

whenMaybe' :: forall a. (a -> Boolean) -> a -> Maybe a
whenMaybe' pred x = if pred x then Just x else Nothing

unlessMaybe' :: forall a. (a -> Boolean) -> a -> Maybe a
unlessMaybe' = whenMaybe' <<< not

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust x = unsafePartial $ fromJust x
