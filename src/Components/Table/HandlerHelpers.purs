module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, Column, Row, parseColumn, parseRow, showCell)
import App.Components.Table.Models (Action, State, CellMove)
import App.Utils.DomUtils (selectAllVisibleElements)
import App.Utils.NumberUtils (coalesce)
import Data.Array as Array
import Halogen as H
import Halogen.Aff as HA
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Element (id, scrollHeight, scrollWidth)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Window (scrollBy)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

arrowMove
  :: forall slots o m a
   . MonadAff m
  => IsEvent a
  => a
  -> CellMove
  -> H.HalogenM State Action slots o m Unit

arrowMove ev selectFn = withPrevent ev $ do
  active <- H.gets \st -> st.activeInput
  when (not active) $ selectCell selectFn

selectCell :: forall slots o m. MonadAff m => CellMove -> H.HalogenM State Action slots o m Unit
selectCell cellFn = do
  { selectedCell } <- H.modify \st -> st
    { activeInput = false
    , selectedCell = fromMaybe st.selectedCell
        $ cellFn st.columns st.rows st.selectedCell
    }
  visibleCols <- selectAllVisibleElements $ QuerySelector "th.column-header"
  visibleRows <- selectAllVisibleElements $ QuerySelector "th.row-header"
  goToCell visibleCols visibleRows selectedCell
  pure unit

goToCell :: forall m. MonadEffect m => Array Element -> Array Element -> Cell -> m Unit
goToCell visibleCols visibleRows cell = liftEffect $ do
  cols <- Array.catMaybes <$> traverse ((parseColumn <$> _) <<< id) visibleCols
  rows <- Array.catMaybes <$> traverse ((parseRow <$> _) <<< id) visibleRows
  goToCellHelper cols rows cell visibleCols visibleRows

goToCellHelper :: Array Column -> Array Row -> Cell -> Array Element -> Array Element -> Effect Unit
goToCellHelper cols rows { column, row } visibleCols visibleRows
  | last' cols == Just column = do
      width <- traverse scrollWidth $ head' visibleCols
      scrollBy ((round $ coalesce $ width) + 1) 0 =<< window
  | head' cols == Just column = do
      width <- traverse scrollWidth $ last' visibleCols
      scrollBy (-((round $ coalesce $ width) + 1)) 0 =<< window
  | last' rows == Just row = do
      height <- traverse scrollHeight $ head' visibleRows
      scrollBy 0 ((round $ coalesce $ height) + 1) =<< window
  | head' rows == Just row = do
      height <- traverse scrollHeight $ last' visibleRows
      scrollBy 0 (-((round $ coalesce $ height) + 1)) =<< window
  | otherwise = pure unit

actOnCell :: forall m. MonadAff m => Cell -> (HTMLElement -> Effect Unit) -> Maybe String -> m Unit
actOnCell cell action subElem = do
  element <- liftAff $ HA.selectElement $ QuerySelector $ "td#" <> showCell cell <> foldMap (" " <> _) subElem
  liftEffect $ fromMaybe (pure unit) (action <$> element)

withPrevent :: forall m a b. MonadEffect m => IsEvent a => a -> m b -> m b
withPrevent ev next = prevent ev *> next

prevent :: forall m a. MonadEffect m => IsEvent a => a -> m Unit
prevent ev = liftEffect (preventDefault $ toEvent ev)

class IsEvent :: forall k. k -> Constraint
class IsEvent a

instance IsEvent MouseEvent
instance IsEvent KeyboardEvent
instance IsEvent FocusEvent
instance IsEvent InputEvent
instance IsEvent DragEvent
instance IsEvent WheelEvent

toEvent :: forall a. IsEvent a => a -> Event
toEvent = unsafeCoerce

toKeyboardEvent :: forall a. IsEvent a => a -> KeyboardEvent
toKeyboardEvent = unsafeCoerce
