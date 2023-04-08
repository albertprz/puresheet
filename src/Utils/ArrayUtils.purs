module App.Utils.ArrayUtils where

import Prelude

import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)



infixr 8 range as ..
class Range a where
  range :: a -> a -> Array a

instance Range Char where
  range c1 c2 = mapMaybe fromCharCode $ Array.range (toCharCode c1) (toCharCode c2)

instance Range Int where
  range = Array.range
