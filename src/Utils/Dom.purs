module App.Utils.Dom where

import FatPrelude

import Data.Array as Array
import Data.String.CodePoints as String
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, ParentNode)
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector, querySelector, querySelectorAll)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (HTMLElement, Window, window)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (innerHeight, innerWidth, scrollBy)
import Web.HTML.Window as Window
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent (WheelEvent)

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

querySelectorHelper
  :: forall a
   . (QuerySelector -> ParentNode -> Effect a)
  -> QuerySelector
  -> Effect a
querySelectorHelper function query =
  (function query <<< HTMLDocument.toParentNode <=< Window.document)
    =<< window

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
parseKeyCode str
  | String.length str == 4
  , String.take 3 str == "Key"
  , Just ch <- Array.last $ toCharArray str = CharKeyCode ch
parseKeyCode str = OtherKeyCode str

toEvent :: forall a. IsEvent a => a -> Event
toEvent = unsafeCoerce

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
  | CharKeyCode Char
  | OtherKeyCode String

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

instance IsEvent MouseEvent
instance IsEvent KeyboardEvent
instance IsEvent InputEvent
instance IsEvent DragEvent
instance IsEvent WheelEvent
