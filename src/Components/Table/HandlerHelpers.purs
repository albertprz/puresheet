module App.Components.Table.HandlerHelpers where

import FatPrelude

import App.Components.Table.Cell (Cell, showCell)
import App.Components.Table.Models (Action, State, CellMove)
import Halogen as H
import Halogen.Aff as HA
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

arrowMove :: forall slots o m a. MonadAff m => AnyEvent a => a -> CellMove
             -> H.HalogenM State Action slots o m Unit
arrowMove ev selectFn = do
  active <- H.gets \st -> st.activeInput
  when (not active) $ withPrevent ev $ selectCell selectFn

selectCell :: forall slots o m. MonadAff m => CellMove -> H.HalogenM State Action slots o m Unit
selectCell cellFn = do
  { cell, columns, rows } <- H.gets \st ->
    { cell: st.selectedCell, columns: st.columns, rows: st.rows }
  let newCell = fromMaybe cell $ cellFn columns rows cell
  H.modify_ \st -> st
    { selectedCell = newCell
    , activeInput = false
    }
  actOnCell newCell focus Nothing

actOnCell :: forall m. MonadAff m => Cell -> (HTMLElement -> Effect Unit) -> Maybe String -> m Unit
actOnCell cell action subElem = do
  element <- H.liftAff $ HA.selectElement $ QuerySelector $ "td#" <> showCell cell <> maybe "" (" " <> _) subElem
  H.liftEffect $ fromMaybe (pure unit) (action <$> element)

withPrevent :: forall m a b. MonadEffect m => AnyEvent a => a -> m b -> m b
withPrevent ev next = prevent ev *> next

prevent :: forall m a. MonadEffect m => AnyEvent a => a -> m Unit
prevent ev = liftEffect (preventDefault $ toEvent ev)

class AnyEvent :: forall k. k -> Constraint
class AnyEvent a

instance AnyEvent MouseEvent
instance AnyEvent KeyboardEvent
instance AnyEvent FocusEvent
instance AnyEvent InputEvent
instance AnyEvent DragEvent
instance AnyEvent WheelEvent

toEvent :: forall a. AnyEvent a => a -> Event
toEvent = unsafeCoerce

toKeyboardEvent :: forall a. AnyEvent a => a -> KeyboardEvent
toKeyboardEvent = unsafeCoerce
