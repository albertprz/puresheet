module App.CSS.Table where

import Prelude
import Tecton

import Color (Color, black, fromHexString)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Tecton.Rule as Rule



hex :: String -> Color
hex = fromMaybe black <<< fromHexString


grey :: Color
grey = hex "#cbcbcb"


css :: CSS
css = do
  tableCss
  cellCss


tableCss :: CSS
tableCss = do
  table ? Rule.do
    width := pct 100
    borderStyle := solid
    borderWidth := px 1
    borderColor := grey


cellCss :: CSS
cellCss = do
  th /\ td ? Rule.do
    borderStyle := solid
    borderColor := grey
    borderWidth := px 0 ~ px 0 ~ px 1 ~ px 1
    fontSize := inherit
    margin := px 0
    overflow := visible
    padding := em 0.5 ~ em 1
