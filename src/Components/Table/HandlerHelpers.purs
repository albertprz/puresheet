module App.Components.Table.HandlerHelpers where

import FatPrelude

import App.Components.Table.Cell (Cell, showCell)
import Halogen as H
import Halogen.Aff as HA
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (HTMLElement)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

actOnCell :: forall m. MonadAff m => Cell -> (HTMLElement -> Effect Unit) -> m Unit
actOnCell cell action = do
  element <- H.liftAff $ HA.selectElement $ QuerySelector $ "td#" <> showCell cell
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
