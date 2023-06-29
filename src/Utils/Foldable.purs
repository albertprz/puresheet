module App.Utils.Foldable where

import Bookhound.Utils.Foldable (hasNone)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))

wrapMaybe :: forall a t. Foldable t => t a -> Maybe (t a)
wrapMaybe x = if hasNone x then Just x else Nothing
