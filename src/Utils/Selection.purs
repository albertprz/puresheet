module App.Utils.Selection where

import FatPrelude

import App.Utils.Common (refEquals)
import App.Utils.Range (Range)
import App.Utils.Range as Range
import Data.String.CodeUnits (length) as String
import Web.DOM (Node)
import Web.DOM.Element (localName)
import Web.DOM.Element as Element
import Web.DOM.Node (childNodes, firstChild, nextSibling, textContent)
import Web.DOM.NodeList as NodeList
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (Window)

getCaretPosition :: Selection -> Node -> Effect (Maybe Int)
getCaretPosition selection parentNode = runMaybeT do
  childNode <- MaybeT $ firstChild parentNode
  MaybeT $ join $ go childNode
    <$> anchorNode selection
    <*> anchorOffset selection
  where
  go node anchor position = do
    len <- String.length <$> textContent node
    textNode <- getChildOrNode node
    nodeText <- fold <$> traverse innerText (HTMLElement.fromNode textNode)
    anchorText <- fold <$> traverse innerText (HTMLElement.fromNode textNode)
    let isFont = any (eq "font" <<< localName) (Element.fromNode textNode)
    if
      any (refEquals anchor) [ node, textNode ]
        || (isFont && nodeText == anchorText) then do
      children <- filterMap Element.fromNode
        <$> (NodeList.toArray =<< childNodes textNode)
      let lineBreaks = length $ filter (eq "br" <<< localName) children
      pure $ pure (position + lineBreaks)
    else runMaybeT do
      sibling <- MaybeT $ nextSibling node
      MaybeT $ go sibling anchor (position + len)

setCaretPosition :: Selection -> Node -> Int -> Effect Unit
setCaretPosition selection parentNode offset = do
  childNode <- unsafeFromJust <$> firstChild parentNode
  anchor <- anchorNode selection
  traverse_ adjustSelection =<< go childNode anchor offset
  where

  adjustSelection (rangeNode /\ rangeOffset) = do
    range <- Range.createCollapsedRange rangeNode rangeOffset
    resetRange selection range

  go node anchor position = do
    len <- String.length <$> textContent node
    if len >= position then
      pure <<< (_ /\ position) <$> getChildOrNode node
    else runMaybeT do
      sibling <- MaybeT $ nextSibling node
      MaybeT $ go sibling anchor (position - len)

getChildOrNode :: Node -> Effect Node
getChildOrNode node = do
  child <- firstChild node
  pure $ unsafeFromJust (child <|> pure node)

resetRange :: Selection -> Range -> Effect Unit
resetRange selection range = do
  removeAllRanges selection
  addRange selection range

moveToEnd :: Selection -> Node -> Effect Unit
moveToEnd selection parent =
  selectAllChildren selection parent *> collapseToEnd selection

data Selection

foreign import anchorNode :: Selection -> Effect Node

foreign import anchorOffset :: Selection -> Effect Int

foreign import addRange :: Selection -> Range -> Effect Unit

foreign import removeAllRanges :: Selection -> Effect Unit

foreign import selectAllChildren :: Selection -> Node -> Effect Unit

foreign import collapseToEnd :: Selection -> Effect Unit

foreign import getSelection :: Window -> Effect Selection

foreign import innerText :: HTMLElement -> Effect String
