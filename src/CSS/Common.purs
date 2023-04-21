module App.CSS.Common where

import FatPrelude

import Color (Color, fromHexString)
import Color as Color

hex :: String -> Color
hex = fromMaybe black <<< fromHexString

black :: Color
black = Color.black

white :: Color
white = Color.white

darkGrey :: Color
darkGrey = hex "#526066"

grey :: Color
grey = hex "#cbcbcb"

lightGrey :: Color
lightGrey = hex "#e0e0e0"

lighterGrey :: Color
lighterGrey = hex "#f2f2f2"

red :: Color
red = hex "#FF0000"

green :: Color
green = hex "#009933"

lightGreen :: Color
lightGreen = hex "#33ff33"
