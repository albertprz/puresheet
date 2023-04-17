module App.Components.Table.Handler where

import FatPrelude

import App.Components.Table.Cell (nextCell, nextColumnCell, nextRowCell, prevCell, prevColumnCell, prevRowCell)
import App.Components.Table.HandlerHelpers (arrowMove, prevent, selectCell, toKeyboardEvent, withPrevent)
import App.Components.Table.Models (Action(..), State)
import App.Utils.ArrayUtils (switchElements)
import App.Utils.DomUtils (elemsInViewport, selectAllElements, selectAllVisibleElements)
import Data.Map as Map
import Effect.Console (log)
import Halogen as H
import Web.DOM.Element (id)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (toElement)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action slots o m Unit

handleAction Initialize = do
  cell <- H.gets \st -> st.selectedCell
  selectCell (\_ _ _ -> Just cell)

handleAction (WriteCell cell value) =
  H.modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }

handleAction (ClickCell cell ev) = do
  selectedCell <- H.gets \st -> st.selectedCell

  arrowMove ev (\_ _ _ -> Just cell)
  when (cell /= selectedCell) $ H.modify_ \st -> st
    { activeInput = false }

handleAction (DoubleClickCell cell ev) = do
  withPrevent ev $ selectCell (\_ _ _ -> Just cell)
  handleAction $ KeyPress "Enter" $ toKeyboardEvent ev

handleAction (KeyPress "Tab" ev) =
  withPrevent ev $ selectCell selectFn
  where
  selectFn
    | KeyboardEvent.shiftKey ev = prevCell
    | otherwise = nextCell

handleAction (KeyPress "ArrowLeft" ev) =
  arrowMove ev prevColumnCell

handleAction (KeyPress "ArrowRight" ev) =
  arrowMove ev nextColumnCell

handleAction (KeyPress "ArrowUp" ev) =
  arrowMove ev prevRowCell

handleAction (KeyPress "ArrowDown" ev) =
  arrowMove ev nextRowCell

handleAction (KeyPress "Space" ev) =
  prevent ev

handleAction (KeyPress "Enter" _) = do
  active <- H.gets \st -> st.activeInput
  H.modify_ \st -> st
    { activeInput = not active }
-- cell <- H.gets \st -> st.selectedCell
-- actOnCell cell focus $ whenMonoid (not active) $ Just "input"

handleAction (KeyPress _ _) = liftEffect $ do
  visibleElems <- selectAllVisibleElements $ QuerySelector "th.column-header"
  -- pure unit
  traverse_ (log <=< id) visibleElems

handleAction (InputKeyPress _ _) =
  pure unit

handleAction (DragHeader startHeader) =
  H.modify_ \st -> st
    { draggedHeader = Just startHeader }

handleAction (DropHeader endHeader) =
  H.modify_ \st -> st
    { columns = maybe st.columns (\col -> switchElements col endHeader st.columns) st.draggedHeader
    , draggedHeader = Nothing
    }

handleAction (DragOverHeader ev) =
  prevent ev

