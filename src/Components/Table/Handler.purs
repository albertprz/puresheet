module App.Components.Table.Handler where

import FatPrelude

import App.CSS.Ids (formulaBoxId, formulaCellInputId, inputElement, selectedCellInputId)
import App.Components.Table.Cell (CellMove(..), Header(..), getColumnHeader, getRowHeader, mkColumn, mkRow, swapTableMapColumn, swapTableMapRow)
import App.Components.Table.Formula (FormulaState(..))
import App.Components.Table.HandlerHelpers (cellArrowMove, cellMove, copyCells, deleteCells, insertFormula, loadPrelude, lookupFormula, pasteCells, refreshCells, selectAllCells, selectCell, subscribeSelectionChange, subscribeWindowResize)
import App.Components.Table.Models (Action(..), AppState, EventTransition(..))
import App.Components.Table.Selection (MultiSelection(..), SelectionState(..))
import App.Evaluator.Formula (mkLocalContext)
import App.Utils.Dom (KeyCode(..), actOnElementById, ctrlKey, displayFunctionType, emptyFormulaBox, emptyFormulaSignature, focusById, focusCell, focusCellElem, introduceFormulaNewLine, isModifierKeyCode, performSyntaxHighlight, prevent, shiftKey, toEvent, toMouseEvent, updateFormulaBox, withPrevent)
import App.Utils.HashMap (lookup2) as HashMap
import App.Utils.Selection (getSelection)
import App.Utils.Selection as Selection
import Data.HashMap (insert) as HashMap
import Data.Set as Set
import Halogen (HalogenM)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLElement (setContentEditable, toNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (scroll)
import Web.UIEvent.WheelEvent (deltaX, deltaY)

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> HalogenM AppState Action slots o m Unit

handleAction Initialize = do
  loadPrelude
  actOnElementById formulaBoxId $ setContentEditable "true"
  handleAction ResizeWindow
  subscribeSelectionChange
  subscribeWindowResize

handleAction ResizeWindow = do
  { selectedCell } <- get
  void $ selectCell $ OtherCell { column: mkColumn 'A', row: mkRow 1000 }
  void $ selectCell $ OtherCell { column: mkColumn 'A', row: mkRow 1 }
  void $ selectCell $ OtherCell selectedCell
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
  modify_ \st -> st
    { tableData = HashMap.insert cell value st.tableData
    , activeInput = false
    }
  refreshCells $ Set.singleton cell

handleAction (FormulaKeyDown Enter ev)
  | ctrlKey ev = withPrevent ev insertFormula

handleAction (FormulaKeyDown Enter ev) =
  withPrevent ev (introduceFormulaNewLine *> performSyntaxHighlight)

handleAction (FormulaKeyDown Tab ev) =
  withPrevent ev $ focusCell =<< gets _.selectedCell

handleAction (FormulaKeyDown (CharKeyCode 'G') ev)
  | ctrlKey ev = withPrevent ev $ focusById formulaCellInputId

handleAction (FormulaKeyDown _ _) =
  modify_ _ { formulaState = UnknownFormula }

handleAction (FormulaKeyUp keyCode _) =
  unless (isModifierKeyCode keyCode)
    performSyntaxHighlight

handleAction (FocusInFormula ev) = do
  selection <- liftEffect $ getSelection =<< window
  liftEffect $ traverse_ (Selection.moveToEnd selection) formulaBox
  whenM (not <$> gets _.activeFormula)
    ( modify_ \st -> st
        { activeFormula = true
        , formulaCell = maybe st.selectedCell _.startingCell $
            HashMap.lookup2 st.selectedCell st.tableFormulas st.formulaCache
        }
    )
  where
  formulaBox =
    toNode <$> (HTMLElement.fromEventTarget =<< (target $ toEvent ev))

handleAction (ClickCell cell ev) = withPrevent ev do
  { selectedCell } <- get
  cellMove ev (OtherCell cell)
  when (cell /= selectedCell) $ modify_ _ { activeInput = false }

handleAction (DoubleClickCell cell ev) = withPrevent ev do
  selectCell (OtherCell cell)
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  focusCellElem selectedCell $ whenMaybe activeInput inputElement

handleAction (FocusInCell cell _) = do
  formulaText <- _.formulaText <$$> lookupFormula cell
  emptyFormulaSignature
  case formulaText of
    Just x -> updateFormulaBox x
    Nothing -> emptyFormulaBox
  modify_ _
    { activeFormula = false
    , formulaState =
        if isJust formulaText then ValidFormula
        else UnknownFormula
    }

handleAction (KeyDown x ev) | x `elem` [ ArrowLeft, CharKeyCode 'H' ] =
  cellArrowMove ev PrevColumn

handleAction (KeyDown x ev) | x `elem` [ ArrowRight, CharKeyCode 'L' ] =
  cellArrowMove ev NextColumn

handleAction (KeyDown x ev) | x `elem` [ ArrowUp, CharKeyCode 'K' ] =
  cellArrowMove ev PrevRow

handleAction (KeyDown x ev) | x `elem` [ ArrowDown, CharKeyCode 'J' ] =
  cellArrowMove ev NextRow

handleAction (KeyDown Enter ev)
  | ctrlKey ev = withPrevent ev do
      modify_ _
        { activeInput = false
        , selectionState = NotStartedSelection
        }
      focusById formulaBoxId
  | otherwise = withPrevent ev do
      { selectedCell, activeInput } <- modify \st -> st
        { activeInput = not st.activeInput }
      focusCellElem selectedCell $ whenMaybe activeInput inputElement

handleAction (KeyDown Tab ev) = withPrevent ev $ selectCell move
  where
  move
    | shiftKey ev = PrevCell
    | otherwise = NextCell

handleAction (KeyDown Space _) = do
  { selectedCell } <- modify _ { activeInput = true }
  focusCellElem selectedCell $ Just inputElement

handleAction (KeyDown Delete _) =
  deleteCells

handleAction (KeyDown Shift ev) = prevent ev

handleAction (KeyDown Control ev) = withPrevent ev $
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

handleAction (KeyRelease Control ev) = withPrevent ev $
  modify_ _ { selectionState = NotStartedSelection }

handleAction (KeyRelease _ _) =
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
  modify_ \st ->
    st
      { tableData = maybe st.tableData
          (\col -> swapTableMapColumn col newColumn st.tableData)
          (st.draggedHeader >>= getColumnHeader)
      , draggedHeader = Nothing
      }
  handleAction $ ClickHeader header (toMouseEvent ev)

handleAction (DragHeader End header@(RowHeader newRow) ev) = do
  modify_ \st ->
    st
      { tableData = maybe st.tableData
          (\row -> swapTableMapRow row newRow st.tableData)
          (st.draggedHeader >>= getRowHeader)
      , draggedHeader = Nothing
      }
  handleAction $ ClickHeader header (toMouseEvent ev)

handleAction (DragHeader Over _ ev) =
  prevent ev

handleAction (DragHeader _ _ _) =
  pure unit

handleAction SelectionChange =
  displayFunctionType <<< mkLocalContext =<< get
