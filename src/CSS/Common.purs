module App.CSS.Common where

import Prelude

import Color (Color, black, fromHexString)
import Data.Maybe (fromMaybe)


hex :: String -> Color
hex = fromMaybe black <<< fromHexString


darkGrey :: Color
darkGrey = hex "#526066"

grey :: Color
grey = hex "#cbcbcb"

lightGrey :: Color
lightGrey = hex "#e0e0e0"

lighterGrey :: Color
lighterGrey = hex "#f2f2f2"
