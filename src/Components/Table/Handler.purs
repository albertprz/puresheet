module App.Components.Table.Handler where

import FatPrelude

import App.Components.Table.Cell
  ( CellMove(..)
  , Header(..)
  , MultiSelection(..)
  , SelectionState(..)
  , getColumnHeader
  , getRowHeader
  , swapTableMapColumn
  , swapTableMapRow
  )
import App.Components.Table.HandlerHelpers
  ( actOnCell
  , cellArrowMove
  , cellMove
  , copyCells
  , deleteCells
  , initialize
  , pasteCells
  , selectAllCells
  , selectCell
  )
import App.Components.Table.Models (Action(..), AppState, EventTransition(..))
import App.Utils.Dom (KeyCode(..), ctrlKey, prevent, shiftKey, withPrevent)
import Data.Map as Map
import Halogen as H
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.WheelEvent (deltaX, deltaY)

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM AppState Action slots o m Unit

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

handleAction (DoubleClickCell cell ev) = withPrevent ev do
  selectCell (OtherCell cell)
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  actOnCell selectedCell focus $ toMaybe' activeInput "input"

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
  actOnCell selectedCell focus $ toMaybe' activeInput "input"

handleAction (KeyPress Tab ev) =
  withPrevent ev $ selectCell move
  where
  move
    | shiftKey ev = PrevCell
    | otherwise = NextCell

handleAction (KeyPress Space ev) = withPrevent ev do
  { selectedCell } <- modify _ { activeInput = true }
  actOnCell selectedCell focus $ Just "input"

handleAction (KeyPress Delete ev) =
  deleteCells ev

handleAction (KeyPress Shift ev) = withPrevent ev $
  modify_ _ { selectionState = InProgressSelection }

handleAction (KeyPress (CharKeyCode 'A') ev)
  | ctrlKey ev = selectAllCells ev

handleAction (KeyPress (CharKeyCode 'C') ev)
  | ctrlKey ev = copyCells ev

handleAction (KeyPress (CharKeyCode 'V') ev)
  | ctrlKey ev = pasteCells ev

handleAction (KeyPress (CharKeyCode 'X') ev)
  | ctrlKey ev = copyCells ev *> deleteCells ev

handleAction (KeyPress (CharKeyCode _) _) =
  pure unit

handleAction (KeyPress (OtherKeyCode _) _) =
  pure unit

handleAction (KeyRelease Shift ev) = withPrevent ev $
  modify_ _ { selectionState = NotStartedSelection }

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

handleAction (ClickHeader CornerHeader ev) =
  selectAllCells ev

handleAction (ClickHeader (ColumnHeader col) _) = do
  selectCell (OtherColumn col)
  modify_ _
    { multiSelection = ColumnsSelection col col
    , selectionState = NotStartedSelection
    }

handleAction (ClickHeader (RowHeader row) _) = do
  selectCell (OtherRow row)
  modify_ _
    { multiSelection = RowsSelection row row
    , selectionState = NotStartedSelection
    }

handleAction (HoverCell Start startCell _) = do
  selectCell (OtherCell startCell)
  modify_ _ { selectionState = InProgressSelection }

handleAction (HoverCell End _ ev) =
  when (not $ shiftKey ev) $
    modify_ _ { selectionState = NotStartedSelection }

handleAction (HoverCell Over overCell _) = do
  { selectionState, multiSelection, selectedCell } <- get
  when (selectionState == InProgressSelection) $
    case multiSelection of
      NoSelection ->
        modify_ _ { multiSelection = CellsSelection overCell overCell }
      CellsSelection _ _ ->
        modify_ _ { multiSelection = CellsSelection selectedCell overCell }
      RowsSelection origin _ ->
        modify_ _ { multiSelection = RowsSelection origin overCell.row }
      ColumnsSelection origin _ ->
        modify_ _ { multiSelection = ColumnsSelection origin overCell.column }
      _ -> pure unit

handleAction (HoverHeader Start _ _) =
  modify_ _ { selectionState = InProgressSelection }

handleAction (HoverHeader End _ ev) =
  when (not $ shiftKey ev) $
    modify_ _ { selectionState = NotStartedSelection }

handleAction (HoverHeader Over (RowHeader overRow) _) = do
  { selectionState, multiSelection } <- get
  when (selectionState == InProgressSelection) $
    case multiSelection of
      NoSelection ->
        modify_ _ { multiSelection = RowsSelection overRow overRow }
      RowsSelection origin _ ->
        modify_ _ { multiSelection = RowsSelection origin overRow }
      _ -> pure unit

handleAction (HoverHeader Over (ColumnHeader overColumn) _) = do
  { selectionState, multiSelection } <- get
  when (selectionState == InProgressSelection) $
    case multiSelection of
      NoSelection ->
        modify_ _ { multiSelection = ColumnsSelection overColumn overColumn }
      ColumnsSelection origin _ ->
        modify_ _ { multiSelection = ColumnsSelection origin overColumn }
      _ -> pure unit

handleAction (HoverHeader Over CornerHeader _) = pure unit

handleAction (DragHeader Start header _) =
  modify_ _ { draggedHeader = Just header }

handleAction (DragHeader End (ColumnHeader newColumn) _) =
  modify_ \st -> st
    { tableData = maybe st.tableData
        (\col -> swapTableMapColumn col newColumn st.tableData)
        (st.draggedHeader >>= getColumnHeader)
    , draggedHeader = Nothing
    }

handleAction (DragHeader End (RowHeader newRow) _) =
  modify_ \st -> st
    { tableData = maybe st.tableData
        (\row -> swapTableMapRow row newRow st.tableData)
        (st.draggedHeader >>= getRowHeader)
    , draggedHeader = Nothing
    }

handleAction (DragHeader Over _ ev) =
  prevent ev

handleAction (DragHeader _ _ _) =
  pure unit
