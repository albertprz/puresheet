module App.Utils.DomUtils where

import FatPrelude

import Web.DOM (Element, ParentNode)
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector, querySelector, querySelectorAll)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (innerHeight, innerWidth)
import Web.HTML.Window as Window

selectAllVisibleElements :: forall m. MonadEffect m => QuerySelector -> m (Array Element)
selectAllVisibleElements query = liftEffect $ do
  elems <- selectAllElements query
  visibleElems <- sequence $ elemsInViewport <$> (toElement <$$> elems)
  pure $ fold visibleElems

selectAllElements :: forall m. MonadEffect m => QuerySelector -> m (Maybe (NonEmptyArray HTMLElement))
selectAllElements query = liftEffect $ do
  nodes <- querySelectorHelper querySelectorAll query
  elems <- NodeList.toArray nodes
  pure $ (traverse HTMLElement.fromNode) =<< fromArray elems

selectElement :: forall m. MonadEffect m => QuerySelector -> m (Maybe HTMLElement)
selectElement query = liftEffect $ do
  maybeElem <- querySelectorHelper querySelector query
  pure $ HTMLElement.fromElement =<< maybeElem

querySelectorHelper :: forall a. (QuerySelector -> ParentNode -> Effect a) -> QuerySelector -> Effect a
querySelectorHelper function query = (function query <<< HTMLDocument.toParentNode <=< Window.document)
  =<< window

elemsInViewport :: forall m. MonadEffect m => NonEmptyArray Element -> m (Array Element)
elemsInViewport elems = liftEffect $ do
  w <- window
  wHeight <- Height <<< toNumber <$> innerHeight w
  wWidth <- Width <<< toNumber <$> innerWidth w
  filterA (isInViewport wHeight wWidth) elems

isInViewport :: forall m. MonadEffect m => Height -> Width -> Element -> m Boolean
isInViewport (Height wHeight) (Width wWidth) element =
  liftEffect $ do
    rect <- getBoundingClientRect element
    let
      visibleY = rect.top < wHeight && pos rect.bottom
      visibleX = rect.left < wWidth && pos rect.right
    pure $ visibleX && visibleY

newtype Height = Height Number
newtype Width = Width Number
