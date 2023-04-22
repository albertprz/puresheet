module App.Components.Table.Handler where

import FatPrelude

import App.Components.Table.Cell (CellMove(..), MultiSelection(..), SelectionState(..))
import App.Components.Table.HandlerHelpers (actOnCell, cellArrowMove, cellMove, initialize, prevent, selectCell, withPrevent)
import App.Components.Table.Models (Action(..), EventTransition(..), Header(..), KeyCode(..), State)
import Data.Map as Map
import Halogen as H
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (shiftKey)
import Web.UIEvent.WheelEvent (deltaX, deltaY)

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action slots o m Unit

handleAction Initialize =
  initialize

handleAction (WriteCell cell value) =
  modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }

handleAction (ClickCell cell ev) = withPrevent ev do
  { selectedCell } <- get
  cellMove ev (OtherCell cell)
  when (cell /= selectedCell) $ modify_ _ { activeInput = false }

handleAction (DoubleClickCell cell ev) = do
  withPrevent ev $ selectCell (OtherCell cell)
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  actOnCell selectedCell focus $ whenMonoid activeInput $ Just "input"

handleAction (KeyPress ArrowLeft ev) =
  cellArrowMove ev PrevColumn

handleAction (KeyPress ArrowRight ev) =
  cellArrowMove ev NextColumn

handleAction (KeyPress ArrowUp ev) =
  cellArrowMove ev PrevRow

handleAction (KeyPress ArrowDown ev) =
  cellArrowMove ev NextRow

handleAction (KeyPress Enter ev) = withPrevent ev do
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  actOnCell selectedCell focus $ whenMonoid activeInput $ Just "input"

handleAction (KeyPress Tab ev) =
  withPrevent ev $ selectCell move
  where
  move
    | KeyboardEvent.shiftKey ev = PrevCell
    | otherwise = NextCell

handleAction (KeyPress Space ev) = withPrevent ev do
  { selectedCell } <- modify _ { activeInput = true }
  actOnCell selectedCell focus $ Just "input"

handleAction (KeyPress Delete ev) = withPrevent ev $
  modify_ \st -> st
    { tableData = Map.delete st.selectedCell st.tableData }

handleAction (KeyPress Shift ev) = withPrevent ev $
  modify_ _ { selectionState = InProgressSelection }

handleAction (KeyPress (OtherKey _) _) =
  pure unit

handleAction (KeyRelease Shift ev) = withPrevent ev $
  modify_ _ { selectionState = FinishedSelection }

handleAction (KeyRelease _ _) =
  pure unit

handleAction (InputKeyPress _ _) =
  pure unit

handleAction (WheelScroll ev)
  | neg $ deltaX ev = cellMove ev PrevColumn
  | pos $ deltaX ev = cellMove ev NextColumn
  | neg $ deltaY ev = cellMove ev PrevRow
  | pos $ deltaY ev = cellMove ev NextRow
  | otherwise = pure unit

handleAction (ClickHeader CornerHeader) =
  modify_ _ { multiSelection = AllSelection }

handleAction (ClickHeader (ColumnHeader col)) = do
  selectCell (OtherColumn col)
  modify_ _ { multiSelection = ColumnSelection $ pure col }

handleAction (ClickHeader (RowHeader row)) = do
  selectCell (OtherRow row)
  modify_ _ { multiSelection = RowSelection $ pure row }

handleAction (DragCell Start startCell ev) = withPrevent ev do
  selectCell (OtherCell startCell)
  modify_ _ { selectionState = InProgressSelection }

handleAction (DragCell End _ ev) =
  when (not $ shiftKey ev) $
    modify_ _ { selectionState = FinishedSelection }

handleAction (DragCell Over overCell _) = do
  { selectedCell, selectionState } <- H.get
  when (selectedCell /= overCell && selectionState == InProgressSelection)
    $ modify_ _ { multiSelection = CellsSelection selectedCell overCell }

handleAction (DragHeader Start startHeader _) =
  modify_ _ { draggedHeader = Just startHeader }

handleAction (DragHeader End endHeader _) =
  modify_ \st -> st
    { columns = maybe st.columns (\col -> switchElements col endHeader st.columns) st.draggedHeader
    , draggedHeader = Nothing
    }

handleAction (DragHeader Over _ ev) =
  prevent ev

