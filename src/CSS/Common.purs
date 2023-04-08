module App.CSS.Common where

import Prelude

import Color (Color, fromHexString)
import Color as Color
import Data.Maybe (fromMaybe)


hex :: String -> Color
hex = fromMaybe black <<< fromHexString

black :: Color
black = Color.black

darkGrey :: Color
darkGrey = hex "#526066"

grey :: Color
grey = hex "#cbcbcb"

lightGrey :: Color
lightGrey = hex "#e0e0e0"

lighterGrey :: Color
lighterGrey = hex "#f2f2f2"
