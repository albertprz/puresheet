module App.Utils.Dom where

import FatPrelude

import App.CSS.Ids (ElementId(..), ElementType, cellId, formulaBoxId)
import App.Components.Table.Cell (Cell, showCell)
import Data.Array as Array
import Data.String.CodePoints as String
import Unsafe.Coerce (unsafeCoerce)
import Web.Clipboard (Clipboard, clipboard)
import Web.DOM (Element, ParentNode)
import Web.DOM.Element (getBoundingClientRect, id)
import Web.DOM.Node (childNodes, nodeValue)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.Event (Event, preventDefault, target)
import Web.Event.EventTarget (EventTarget)
import Web.HTML (HTMLElement, Window, window)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus, toElement, toNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (innerHeight, innerWidth, navigator, scrollBy)
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent (WheelEvent)

getFormulaBoxContents :: forall m. MonadEffect m => m String
getFormulaBoxContents = liftEffect do
  formulaBox <- selectElementById formulaBoxId
  nodeList <- traverse (childNodes <<< toNode) formulaBox
  childrenElems <- fold <$> traverse NodeList.toArray nodeList
  childrenTexts <- traverse nodeValue childrenElems
  pure $ foldMap fold childrenTexts

emptyFormulaBox :: forall m. MonadEffect m => m Unit
emptyFormulaBox = do
  formulaBox <- selectElementById formulaBoxId
  traverse_ (removeChildrenText <<< toNode) formulaBox
  where
  removeChildrenText node = liftEffect do
    Node.normalize node
    traverse_ (Node.setTextContent "") =<< Node.firstChild node

parseElements
  :: forall m a
   . MonadEffect m
  => (String -> Maybe a)
  -> Array Element
  -> m (Array a)
parseElements f elems = liftEffect
  (Array.catMaybes <$> traverse ((f <$> _) <<< id) elems)

getVisibleCols :: forall m. MonadEffect m => m (Array Element)
getVisibleCols = selectAllVisibleElements $ QuerySelector "th.column-header"

getVisibleRows :: forall m. MonadEffect m => m (Array Element)
getVisibleRows = selectAllVisibleElements $ QuerySelector "th.row-header"

focusCellElem :: forall m. MonadEffect m => Cell -> Maybe ElementType -> m Unit
focusCellElem cell subElem = actOnCellElem cell focus subElem

focusCell :: forall m. MonadEffect m => Cell -> m Unit
focusCell = flip focusCellElem Nothing

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
actOnElementById id action = do
  element <- selectElementById id
  liftEffect $ traverse_ action element

selectAllVisibleElements
  :: forall m. MonadEffect m => QuerySelector -> m (Array Element)
selectAllVisibleElements query = liftEffect $ do
  elems <- selectAllElements query
  visibleElems <- sequence $ elemsInViewport <$> (toElement <$$> elems)
  pure $ fold visibleElems

selectAllElements
  :: forall m
   . MonadEffect m
  => QuerySelector
  -> m (Maybe (NonEmptyArray HTMLElement))
selectAllElements query = liftEffect $ do
  nodes <- querySelectorHelper querySelectorAll query
  elems <- NodeList.toArray nodes
  pure $ (traverse HTMLElement.fromNode) =<< fromArray elems

selectElement
  :: forall m. MonadEffect m => QuerySelector -> m (Maybe HTMLElement)
selectElement query = liftEffect $ do
  maybeElem <- querySelectorHelper querySelector query
  pure $ HTMLElement.fromElement =<< maybeElem

selectElementById
  :: forall m. MonadEffect m => ElementId -> m (Maybe HTMLElement)
selectElementById = selectElement <<< QuerySelector <<< ("#" <> _) <<< show

querySelectorHelper
  :: forall a
   . (QuerySelector -> ParentNode -> Effect a)
  -> QuerySelector
  -> Effect a
querySelectorHelper function query =
  (function query <<< HTMLDocument.toParentNode <=< Window.document)
    =<< window

getClipboard :: forall m. MonadEffect m => m Clipboard
getClipboard = liftEffect $ clipboard =<< navigator =<< window

elemsInViewport
  :: forall m. MonadEffect m => NonEmptyArray Element -> m (Array Element)
elemsInViewport elems = liftEffect $ do
  w <- window
  wHeight <- Height <<< toNumber <$> innerHeight w
  wWidth <- Width <<< toNumber <$> innerWidth w
  filterA (isInViewport wHeight wWidth) elems

isInViewport
  :: forall m. MonadEffect m => Height -> Width -> Element -> m Boolean
isInViewport (Height wHeight) (Width wWidth) element =
  liftEffect $ do
    rect <- getBoundingClientRect element
    let
      visibleY = rect.top < wHeight && pos rect.bottom
      visibleX = rect.left < wWidth && pos rect.right
    pure $ visibleX && visibleY

withPrevent :: forall m a b. MonadEffect m => IsEvent a => a -> m b -> m b
withPrevent ev next = prevent ev *> next

prevent :: forall m a. MonadEffect m => IsEvent a => a -> m Unit
prevent ev = liftEffect (preventDefault $ toEvent ev)

scrollByX :: Number -> Window -> Effect Unit
scrollByX x = scrollBy' x 0.0

scrollByY :: Number -> Window -> Effect Unit
scrollByY = scrollBy' 0.0

scrollBy' :: Number -> Number -> Window -> Effect Unit
scrollBy' x y = scrollBy (unsafeCoerce x) (unsafeCoerce y)

parseKeyCode :: String -> KeyCode
parseKeyCode "ArrowLeft" = ArrowLeft
parseKeyCode "ArrowRight" = ArrowRight
parseKeyCode "ArrowUp" = ArrowUp
parseKeyCode "ArrowDown" = ArrowDown
parseKeyCode "Enter" = Enter
parseKeyCode "Tab" = Tab
parseKeyCode "Space" = Space
parseKeyCode "Delete" = Delete
parseKeyCode "Backspace" = Delete
parseKeyCode "ShiftLeft" = Shift
parseKeyCode "ShiftRight" = Shift
parseKeyCode "ControlLeft" = Control
parseKeyCode "ControlRight" = Control
parseKeyCode "MetaLeft" = Control
parseKeyCode "MetaRight" = Control
parseKeyCode str
  | String.length str == 4
  , String.take 3 str == "Key"
  , Just ch <- last' $ toCharArray str = CharKeyCode ch
parseKeyCode str = OtherKeyCode str

toEvent :: forall a. IsEvent a => a -> Event
toEvent = unsafeCoerce

getTarget :: forall a. IsEvent a => a -> Maybe EventTarget
getTarget = target <<< toEvent

toMouseEvent :: forall a. IsEvent a => a -> MouseEvent
toMouseEvent = unsafeCoerce

newtype Height = Height Number
newtype Width = Width Number

data KeyCode
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Enter
  | Tab
  | Space
  | Delete
  | Shift
  | Control
  | CharKeyCode Char
  | OtherKeyCode String

derive instance Eq KeyCode

class ModKeyEvent a where
  shiftKey :: a -> Boolean
  ctrlKey :: a -> Boolean

instance ModKeyEvent KeyboardEvent where
  shiftKey = KeyboardEvent.shiftKey
  ctrlKey ev = KeyboardEvent.ctrlKey ev || KeyboardEvent.metaKey ev

instance ModKeyEvent MouseEvent where
  shiftKey = MouseEvent.shiftKey
  ctrlKey ev = MouseEvent.ctrlKey ev || MouseEvent.metaKey ev

class IsEvent :: forall k. k -> Constraint
class IsEvent a

instance IsEvent FocusEvent
instance IsEvent MouseEvent
instance IsEvent KeyboardEvent
instance IsEvent InputEvent
instance IsEvent DragEvent
instance IsEvent WheelEvent
