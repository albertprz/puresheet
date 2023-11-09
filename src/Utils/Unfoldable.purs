module App.Utils.Unfoldable where

import Prelude

import App.Utils.Maybe (unsafeFromJust)
import Bookhound.FatPrelude (fromCharCode)
import Data.Char (toCharCode)
import Data.Traversable (class Traversable, traverse)
import Data.Unfoldable (class Unfoldable1)
import Data.Unfoldable1 as Unfoldable1

infixr 8 range as ..

class Range a where
  range
    :: forall f. Unfoldable1 f => Traversable f => a -> a -> f a

instance Range Int where
  range = Unfoldable1.range

instance Range Char where
  range c1 c2 = unsafeFromJust $ traverse fromCharCode $
    range (toCharCode c1) (toCharCode c2)
