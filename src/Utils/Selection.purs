module App.Utils.Selection where

import FatPrelude

import App.Utils.Range (Range)
import Web.DOM (Node)

data Selection

resetRange :: Selection -> Range -> Effect Unit
resetRange selection range = do
  removeAllRanges selection
  addRange selection range

foreign import anchorNode :: Selection -> Effect Node

foreign import anchorOffset :: Selection -> Effect Int

foreign import addRange :: Selection -> Range -> Effect Unit

foreign import removeAllRanges :: Selection -> Effect Unit
