module App.Components.Spreadsheet.Handler where

import FatPrelude

import App.AppM (AppM)
import App.CSS.Ids (cellId, formulaBoxId, inputElementType, selectedCellInputId)
import App.Components.Editor (_editor)
import App.Components.Editor.Models (EditorOutput, EditorQuery(..))
import App.Components.Editor.Models as EditorOutput
import App.Components.Spreadsheet.Cell (CellMove(..), Header(..), getColumnHeader, getRowHeader, mkColumn, mkRow, swapTableMapColumn, swapTableMapRow)
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Components.Spreadsheet.HandlerHelpers (cellArrowMove, cellMove, copyCells, deleteCells, focusCell, focusCellElem, insertFormula, lookupFormula, pasteCells, refreshCells, selectAllCells, selectCell, subscribeWindowResize)
import App.Components.Spreadsheet.Models (EventTransition(..), Slots, SpreadsheetAction(..), SpreadsheetState)
import App.Components.Spreadsheet.Selection (MultiSelection(..), SelectionState(..))
import App.Utils.Dom (focusById, prevent, withPrevent)
import App.Utils.Event (ctrlKey, shiftKey, toMouseEvent)
import App.Utils.HashMap (lookup2) as HashMap
import App.Utils.KeyCode (KeyCode(..))
import Data.HashMap (insert) as HashMap
import Data.Set as Set
import Halogen (HalogenM)
import Halogen.Query (tellAll)
import Halogen.Store.Monad (updateStore)
import Record (merge)
import Web.HTML (window)
import Web.HTML.Window (scroll)
import Web.UIEvent.WheelEvent (deltaX, deltaY)

handleAction
  :: forall o
   . SpreadsheetAction
  -> HalogenM SpreadsheetState SpreadsheetAction Slots o AppM Unit

handleAction Initialize = do
  handleAction ResizeWindow
  subscribeWindowResize
  applyFocus

handleAction (Receive { context, input }) = do
  modify_ (merge context <<< merge input)
  applyFocus

handleAction ResizeWindow = do
  { selectedCell } <- get
  selectCell $ OtherCell { column: mkColumn 'A', row: mkRow 1000 }
  selectCell $ OtherCell { column: mkColumn 'A', row: mkRow 1 }
  selectCell $ OtherCell selectedCell
  liftEffect $ scroll 0 0 =<< window

handleAction (WriteSelectedCellInput cell) =
  traverse_ (selectCell <<< OtherCell) cell

handleAction (WriteFormulaCellInput cell) =
  traverse_ (\x -> modify_ _ { formulaCell = x }) cell

handleAction (SelectedCellInputKeyDown Tab ev) =
  withPrevent ev $ focusCell =<< gets _.selectedCell

handleAction (SelectedCellInputKeyDown _ _) =
  pure unit

handleAction (FormulaCellInputKeyDown Tab ev) =
  withPrevent ev $ focusById formulaBoxId

handleAction (FormulaCellInputKeyDown _ _) =
  pure unit

handleAction (WriteCell cell value) = do
  updateStore \store -> store
    { tableData = HashMap.insert cell value store.tableData }
  modify_ _ { activeInput = false }
  refreshCells $ Set.singleton cell

handleAction FocusInEditor = do
  whenM (not <$> gets _.activeFormula)
    ( modify_ \st -> st
        { activeFormula = true
        , formulaCell = maybe st.selectedCell _.startingCell $
            HashMap.lookup2 st.selectedCell st.tableFormulas st.formulaCache
        }
    )

handleAction FocusOutEditor =
  focusCell =<< gets _.selectedCell

handleAction (SubmitEditor editorText) =
  insertFormula editorText

handleAction (ClickCell cell ev) = withPrevent ev do
  { selectedCell } <- get
  cellMove ev (OtherCell cell)
  when (cell /= selectedCell) $ modify_ _ { activeInput = false }

handleAction (DoubleClickCell cell ev) = withPrevent ev do
  selectCell (OtherCell cell)
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  focusCellElem selectedCell $ whenMaybe activeInput inputElementType

handleAction (FocusInCell cell _) = do
  formulaText <- _.formulaText <$$> lookupFormula cell
  { selectedCell } <- get
  when (cell /= selectedCell)
    $ tellAll _editor
    $ UpdateEditorContent
    $ fromMaybe mempty formulaText
  modify_ _
    { activeFormula = false
    , formulaState =
        if isJust formulaText then ValidFormula
        else UnknownFormula
    }

handleAction (KeyDown x ev)
  | x `elem` [ ArrowLeft, CharKeyCode 'H' ] && not (ctrlKey ev) =
      withPrevent ev $ cellArrowMove ev PrevColumn

handleAction (KeyDown x ev)
  | x `elem` [ ArrowRight, CharKeyCode 'L' ] && not (ctrlKey ev) =
      withPrevent ev $ cellArrowMove ev NextColumn

handleAction (KeyDown x ev)
  | x `elem` [ ArrowUp, CharKeyCode 'K' ] && not (ctrlKey ev) =
      withPrevent ev $ cellArrowMove ev PrevRow

handleAction (KeyDown x ev)
  | x `elem` [ ArrowDown, CharKeyCode 'J' ] && not (ctrlKey ev) =
      withPrevent ev $ cellArrowMove ev NextRow

handleAction (KeyDown Enter ev)
  | ctrlKey ev = withPrevent ev do
      modify_ _
        { activeInput = false
        , selectionState = NotStartedSelection
        }
      focusById formulaBoxId

handleAction (KeyDown Enter ev) = withPrevent ev do
  { selectedCell, activeInput } <-
    modify \st -> st { activeInput = not st.activeInput }
  focusCellElem selectedCell $ whenMaybe activeInput inputElementType

handleAction (KeyDown Tab ev) = withPrevent ev $ selectCell move
  where
  move
    | shiftKey ev = PrevCell
    | otherwise = NextCell

handleAction (KeyDown Space _) = do
  { selectedCell } <- modify _ { activeInput = true }
  focusCellElem selectedCell $ Just inputElementType

handleAction (KeyDown Delete _) =
  deleteCells

handleAction (KeyDown Shift ev) = withPrevent ev $
  modify_ _ { selectionState = InProgressSelection }

handleAction (KeyDown (CharKeyCode 'A') ev)
  | ctrlKey ev = selectAllCells ev

handleAction (KeyDown (CharKeyCode 'C') ev)
  | ctrlKey ev = copyCells ev

handleAction (KeyDown (CharKeyCode 'V') ev)
  | ctrlKey ev = pasteCells ev

handleAction (KeyDown (CharKeyCode 'X') ev)
  | ctrlKey ev = copyCells ev *> deleteCells

handleAction (KeyDown (CharKeyCode 'G') ev)
  | ctrlKey ev = withPrevent ev $ focusById selectedCellInputId

handleAction (KeyDown _ _) =
  pure unit

handleAction (KeyUp Shift ev) = withPrevent ev $
  modify_ _ { selectionState = NotStartedSelection }

handleAction (KeyUp _ _) =
  pure unit

handleAction (WheelScroll ev)
  | neg $ deltaX ev = cellMove ev PrevColumn
  | pos $ deltaX ev = cellMove ev NextColumn
  | neg $ deltaY ev = cellMove ev PrevRow
  | pos $ deltaY ev = cellMove ev NextRow
  | otherwise = pure unit

handleAction (ClickHeader CornerHeader ev) = do
  selectAllCells ev
  focusCell =<< gets _.selectedCell

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

handleAction (HoverCell Start startCell ev) = withPrevent ev do
  selectCell (OtherCell startCell)
  modify_ _ { selectionState = InProgressSelection }

handleAction (HoverCell End _ ev) =
  when (not $ ctrlKey ev) $
    modify_ _ { selectionState = NotStartedSelection }

handleAction (HoverCell Over overCell ev) = withPrevent ev do
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
  pure unit

handleAction (HoverHeader End _ _) =
  pure unit

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

handleAction (DragHeader Start header ev) = do
  modify_ _ { draggedHeader = Just header }
  handleAction $ ClickHeader header (toMouseEvent ev)

handleAction (DragHeader End header@(ColumnHeader newColumn) ev) = do
  { draggedHeader } <- get
  updateStore \store -> store
    { tableData = maybe store.tableData
        (\col -> swapTableMapColumn col newColumn store.tableData)
        (draggedHeader >>= getColumnHeader)
    }
  modify_ _ { draggedHeader = Nothing }
  handleAction $ ClickHeader header (toMouseEvent ev)

handleAction (DragHeader End header@(RowHeader newRow) ev) = do
  { draggedHeader } <- get
  updateStore \store -> store
    { tableData = maybe store.tableData
        (\row -> swapTableMapRow row newRow store.tableData)
        (draggedHeader >>= getRowHeader)
    }
  modify_ _ { draggedHeader = Nothing }
  handleAction $ ClickHeader header (toMouseEvent ev)

handleAction (DragHeader Over _ ev) =
  prevent ev

handleAction (DragHeader _ _ _) =
  pure unit

handleEditorOutput :: EditorOutput -> SpreadsheetAction
handleEditorOutput = case _ of
  EditorOutput.FocusInEditor -> FocusInEditor
  EditorOutput.FocusOutEditor -> FocusOutEditor
  EditorOutput.SubmitEditor x -> SubmitEditor x

applyFocus
  :: forall o s. HalogenM SpreadsheetState SpreadsheetAction s o AppM Unit
applyFocus = do
  cell <- gets _.selectedCell
  focusById $ cellId cell
