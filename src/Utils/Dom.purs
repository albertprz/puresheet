module App.Utils.Dom where

import FatPrelude

import App.CSS.Ids (ElementId(..), ElementType, cellId)
import App.Components.Spreadsheet.Cell (Cell, showCell)
import App.Utils.Event (class IsEvent, toEvent)
import Data.Array (filterA)
import Data.Int as Int
import Halogen.HTML (HTML)
import Halogen.VDom.DOM.StringRenderer as StringRenderer
import Web.Clipboard (Clipboard, clipboard)
import Web.DOM (Element, Node, ParentNode)
import Web.DOM.Document (documentElement)
import Web.DOM.Element (getBoundingClientRect, id, scrollLeft, setAttribute, setScrollLeft)
import Web.DOM.Node (firstChild, nodeName, nodeValue, parentNode, setTextContent)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.Event (preventDefault)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus, toElement, toNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

parseElements
  :: forall m a
   . MonadEffect m
  => (String -> Maybe a)
  -> Array Element
  -> m (Array a)
parseElements parseFn elems = liftEffect
  (filterMap parseFn <$> traverse id elems)

emptyContents :: forall m. ElementId -> MonadEffect m => m Unit
emptyContents elementId = liftEffect
  (setTextContent mempty <<< toNode =<< justSelectElementById elementId)

getVisibleCols :: forall m. MonadEffect m => m (Array Element)
getVisibleCols = selectAllVisibleElements $ QuerySelector "th.column-header"

getVisibleRows :: forall m. MonadEffect m => m (Array Element)
getVisibleRows = selectAllVisibleElements $ QuerySelector "th.row-header"

focusCellElem :: forall m. MonadEffect m => Cell -> Maybe ElementType -> m Unit
focusCellElem cell subElem = actOnCellElem cell focus subElem

focusCell :: forall m. MonadEffect m => Cell -> m Unit
focusCell = flip focusCellElem Nothing

scrollCellRight :: Element -> Effect Unit
scrollCellRight = scrollCell (+)

scrollCellLeft :: Element -> Effect Unit
scrollCellLeft = scrollCell (-)

scrollCell :: (Number -> Number -> Number) -> Element -> Effect Unit
scrollCell f element = do
  traverse_ go =<< getDocumentElement
  where
  go doc = do
    scroll <- scrollLeft doc
    width <- getElemWidth element
    setScrollLeft (f scroll width) doc

focusById :: forall m. MonadEffect m => ElementId -> m Unit
focusById = flip actOnElementById focus

actOnCellElem
  :: forall m
   . MonadEffect m
  => Cell
  -> (HTMLElement -> Effect Unit)
  -> Maybe ElementType
  -> m Unit
actOnCellElem cell action subElem =
  actOnElementById
    ( ElementId
        (show cellId <> showCell cell <> foldMap ((" " <> _) <<< show) subElem)
    )
    action

actOnElementById
  :: forall m
   . MonadEffect m
  => ElementId
  -> (HTMLElement -> Effect Unit)
  -> m Unit
actOnElementById id action = liftEffect
  (traverse_ action =<< selectElementById id)

selectAllVisibleElements
  :: forall m. MonadEffect m => QuerySelector -> m (Array Element)
selectAllVisibleElements query = liftEffect $ do
  elems <- selectAllElements query
  visibleElems <- elemsInViewport $ map toElement elems
  pure visibleElems

selectAllElements
  :: forall m
   . MonadEffect m
  => QuerySelector
  -> m (Array HTMLElement)
selectAllElements query = liftEffect $ do
  nodes <- querySelectorHelper querySelectorAll query
  elems <- NodeList.toArray nodes
  pure $ filterMap HTMLElement.fromNode elems

selectElement
  :: forall m. MonadEffect m => QuerySelector -> m (Maybe HTMLElement)
selectElement query = liftEffect $ do
  maybeElem <- querySelectorHelper querySelector query
  pure $ HTMLElement.fromElement =<< maybeElem

justSelectElementById
  :: forall m. MonadEffect m => ElementId -> m HTMLElement
justSelectElementById x =
  unsafeFromJust <$> selectElementById x

selectElementById
  :: forall m. MonadEffect m => ElementId -> m (Maybe HTMLElement)
selectElementById =
  selectElement <<< QuerySelector <<< ("#" <> _) <<< show

querySelectorHelper
  :: forall a
   . (QuerySelector -> ParentNode -> Effect a)
  -> QuerySelector
  -> Effect a
querySelectorHelper function query =
  function query <<< HTMLDocument.toParentNode =<< getDocument

getDocument :: Effect HTMLDocument
getDocument = Window.document =<< window

getDocumentElement :: Effect (Maybe Element)
getDocumentElement =
  documentElement =<< HTMLDocument.toDocument <$> getDocument

getClipboard :: forall m. MonadEffect m => m Clipboard
getClipboard = liftEffect
  (clipboard =<< Window.navigator =<< window)

getAncestorNodes :: Node -> Effect (Array Node)
getAncestorNodes node = do
  parentNode' <- parentNode node
  grandParentNode <- join <$> traverse parentNode parentNode'
  pure $ compact [ parentNode', grandParentNode ]

getNodeText :: Node -> Effect String
getNodeText node = do
  child <- firstChild node
  childText <- join <$> traverse nodeValue child
  nodeText <- nodeValue node
  pure $ fold (brText <|> nodeText <|> childText)
  where
  brText = whenMaybe (nodeName node == "br") "\n"

elemsInViewport
  :: forall m. MonadEffect m => Array Element -> m (Array Element)
elemsInViewport elems = liftEffect $ do
  w <- window
  wHeight <- Height <<< Int.toNumber <$> Window.innerHeight w
  wWidth <- Width <<< Int.toNumber <$> Window.innerWidth w
  filterA (isInViewport wHeight wWidth) elems

isInViewport
  :: forall m. MonadEffect m => Height -> Width -> Element -> m Boolean
isInViewport (Height wHeight) (Width wWidth) element = liftEffect do
  rect <- getBoundingClientRect element
  let
    visibleY = rect.top < wHeight && pos rect.bottom
    visibleX = rect.left < wWidth && pos rect.right
  pure $ visibleX && visibleY

getElemWidth :: forall m. MonadEffect m => Element -> m Number
getElemWidth element = liftEffect
  (_.width <$> getBoundingClientRect element)

withPrevent :: forall m a b. MonadEffect m => IsEvent a => a -> m b -> m b
withPrevent ev next = prevent ev *> next

prevent :: forall m a. MonadEffect m => IsEvent a => a -> m Unit
prevent ev = liftEffect $ preventDefault $ toEvent ev

setStyle :: Element -> Array (String /\ String) -> Effect Unit
setStyle = flip (setAttribute "style" <<< foldMap (uncurry formatKeyValue))
  where
  formatKeyValue k v = k <> ":" <> v <> ";"

setInnerHTML :: forall a b. Element -> Array (HTML a b) -> Effect Unit
setInnerHTML elem children = setInnerHTML_ elem
  $ fold
  $ StringRenderer.render (const mempty)
  <$> unwrap
  <$> children

foreign import setInnerHTML_ :: Element -> String -> Effect Unit

newtype Height = Height Number
newtype Width = Width Number
