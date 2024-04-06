module App.Utils.Range where

import FatPrelude

import Web.DOM (Node)
import Web.DOM.Element (DOMRect)

createCollapsedRange :: Node -> Int -> Effect Range
createCollapsedRange node offset =
  createRange { start: node /\ offset, end: node /\ offset }

createRange :: { start :: Node /\ Int, end :: Node /\ Int } -> Effect Range
createRange { start, end } = do
  uncurry (setStart range) start
  uncurry (setEnd range) end
  pure range
  where
  range = newRange

data Range

foreign import newRange :: Range

foreign import setStart :: Range -> Node -> Int -> Effect Unit

foreign import setEnd :: Range -> Node -> Int -> Effect Unit

foreign import getBoundingClientRect :: Range -> Effect DOMRect
