module App.Utils.Selection where

import FatPrelude

import App.Utils.Range (Range)
import Web.DOM (Node)

data Selection

resetRange :: Selection -> Range -> Effect Unit
resetRange selection range = do
  removeAllRanges selection
  addRange selection range

moveToEnd :: Selection -> Node -> Effect Unit
moveToEnd selection parent =
  selectAllChildren selection parent *> collapseToEnd selection

foreign import anchorNode :: Selection -> Effect Node

foreign import anchorOffset :: Selection -> Effect Int

foreign import addRange :: Selection -> Range -> Effect Unit

foreign import removeAllRanges :: Selection -> Effect Unit

foreign import selectAllChildren :: Selection -> Node -> Effect Unit

foreign import collapseToEnd :: Selection -> Effect Unit
