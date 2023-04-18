module App.Components.Table.Handler where

import FatPrelude

import App.Components.Table.HandlerHelpers (actOnCell, arrowMove, prevent, selectCell, withPrevent)
import App.Components.Table.Models (Action(..), CellMove(..), Key(..), State)
import Data.Map as Map
import Halogen as H
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.WheelEvent (deltaX, deltaY)

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action slots o m Unit

handleAction Initialize = do
  cell <- H.gets \st -> st.selectedCell
  selectCell (OtherCell cell)

handleAction (WriteCell cell value) =
  H.modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }

handleAction (ClickCell cell ev) = do
  selectedCell <- H.gets \st -> st.selectedCell

  arrowMove ev (OtherCell cell)
  when (cell /= selectedCell) $ H.modify_ \st -> st
    { activeInput = false }

handleAction (DoubleClickCell cell ev) = do
  withPrevent ev $ selectCell (OtherCell cell)
  { selectedCell, activeInput } <- H.modify \st -> st
    { activeInput = not st.activeInput }
  actOnCell selectedCell focus $ whenMonoid activeInput $ Just "input"

handleAction (KeyPress ArrowLeft ev) =
  arrowMove ev PrevColumn

handleAction (KeyPress ArrowRight ev) =
  arrowMove ev NextColumn

handleAction (KeyPress ArrowUp ev) =
  arrowMove ev PrevRow

handleAction (KeyPress ArrowDown ev) =
  arrowMove ev NextRow

handleAction (KeyPress Enter ev) = withPrevent ev $ do
  { selectedCell, activeInput } <- H.modify \st -> st
    { activeInput = not st.activeInput }
  actOnCell selectedCell focus $ whenMonoid activeInput $ Just "input"

handleAction (KeyPress Tab ev) =
  withPrevent ev $ selectCell move
  where
  move
    | KeyboardEvent.shiftKey ev = PrevCell
    | otherwise = NextCell

handleAction (KeyPress Space ev) = withPrevent ev $ do
  { selectedCell } <- H.modify \st -> st
    { activeInput = true }
  actOnCell selectedCell focus $ Just "input"

handleAction (KeyPress (OtherKey _) _) =
  pure unit

handleAction (InputKeyPress _ _) =
  pure unit

handleAction (WheelScroll ev)
  | neg $ deltaX ev = arrowMove ev PrevColumn
  | pos $ deltaX ev = arrowMove ev NextColumn
  | neg $ deltaY ev = arrowMove ev PrevRow
  | pos $ deltaY ev = arrowMove ev NextRow
  | otherwise = pure unit

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

