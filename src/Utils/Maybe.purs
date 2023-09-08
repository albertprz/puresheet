module App.Utils.Maybe where

import Bookhound.Utils.Foldable (hasNone)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))

wrapMaybe :: forall a t. Foldable t => t a -> Maybe (t a)
wrapMaybe x = if hasNone x then Just x else Nothing

toMaybe :: forall a. (a -> Boolean) -> a -> Maybe a
toMaybe pred x = if pred x then Just x else Nothing

toMaybe' :: forall a. Boolean -> a -> Maybe a
toMaybe' cond x = if cond then Just x else Nothing
