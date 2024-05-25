module App.CSS.Common where

import FatPrelude

import Color (Color, fromHexString, rgba)
import Color as Color
import Tecton.Internal (Length, Measure, px)

formulaFontSize :: Measure Length
formulaFontSize = px 21

signatureFontSize :: Measure Length
signatureFontSize = px 28

functionDescriptionFontSize :: Measure Length
functionDescriptionFontSize = px 30

termTypeFontSize :: Measure Length
termTypeFontSize = px 24

inputFontSize :: Measure Length
inputFontSize = px 20

hex :: String -> Color
hex = fromMaybe black <<< fromHexString

black :: Color
black = Color.black

white :: Color
white = Color.white

modalBackground :: Color
modalBackground = rgba 0 0 0 0.5

placeholderColor :: Color
placeholderColor = hex "#8e8e8e"

darkerGrey :: Color
darkerGrey = hex "#526066"

darkGrey :: Color
darkGrey = hex "#bfbfbf"

grey :: Color
grey = hex "#cbcbcb"

lightGrey :: Color
lightGrey = hex "#d9d9d9"

lighterGrey :: Color
lighterGrey = hex "#f2f2f2"

red :: Color
red = hex "#FF0000"

lighterRed :: Color
lighterRed = hex "#ffcccc"

darkGreen :: Color
darkGreen = hex "#006600"

green :: Color
green = hex "#009933"

lightGreen :: Color
lightGreen = hex "#4dff4d"

lighterGreen :: Color
lighterGreen = hex "#ccffcc"

yellow :: Color
yellow = hex "#b3b300"

lighterYellow :: Color
lighterYellow = hex "#ffffcc"

blue :: Color
blue = hex "#0066cc"

lightBlue :: Color
lightBlue = hex "#6699ff"

orange :: Color
orange = hex "#ff6600"

brown :: Color
brown = hex "#993333"

mustard :: Color
mustard = hex "#cc9900"

pink :: Color
pink = hex "#ff3399"

darkPink :: Color
darkPink = hex "#99004d"

purple :: Color
purple = hex "#993399"
