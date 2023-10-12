module App.Components.Table.Handler where

import FatPrelude

import App.CSS.Ids (formulaBoxId, formulaCellInputId, inputElement, selectedCellInputId)
import App.Components.Table.Cell (CellMove(..), Header(..), MultiSelection(..), SelectionState(..), getColumnHeader, getRowHeader, swapTableMapColumn, swapTableMapRow)
import App.Components.Table.Formula (FormulaState(..), newFormulaId, toDependenciesMap)
import App.Components.Table.HandlerHelpers (cellArrowMove, cellMove, copyCells, deleteCells, emptyFormulaBox, focusById, focusCell, focusCellElem, getFormulaBoxContents, initialize, pasteCells, refreshCells, refreshCellsFromDeps, selectAllCells, selectCell)
import App.Components.Table.Models (Action(..), AppState, EventTransition(..))
import App.Interpreter.Formula (runFormula)
import App.Utils.Dom (KeyCode(..), ctrlKey, prevent, shiftKey, toMouseEvent, withPrevent)
import App.Utils.Map (lookup2) as Map
import Data.Map (insert, keys, member, union, unionWith) as Map
import Data.Set as Set
import Effect.Class.Console as Logger
import Halogen as H
import Web.UIEvent.WheelEvent (deltaX, deltaY)

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM AppState Action slots o m Unit

handleAction Initialize =
  initialize

handleAction (WriteSelectedCellInput cell) =
  traverse_ (selectCell <<< OtherCell) cell

handleAction (WriteFormulaCellInput cell) =
  traverse_ (\x -> modify_ _ { formulaCell = x }) cell

handleAction (SelectedCellInputKeyPress Tab ev) =
  withPrevent ev $ focusCell =<< gets _.selectedCell

handleAction (SelectedCellInputKeyPress _ _) =
  pure unit

handleAction (FormulaCellInputKeyPress Tab ev) =
  withPrevent ev $ focusById formulaBoxId

handleAction (FormulaCellInputKeyPress _ _) =
  pure unit

handleAction (WriteCell cell value) = do
  modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }
  refreshCells $ Set.singleton cell

handleAction (FormulaKeyPress Enter ev)
  | ctrlKey ev = withPrevent ev do
      formulaText <- getFormulaBoxContents ev
      st <- get
      case runFormula st st.formulaCell formulaText of
        Right { result, affectedCells, formulaCells, cellDeps } -> do
          let formulaId = newFormulaId $ Map.keys st.formulaCache
          focusCell st.selectedCell
          modify_ _
            { tableData = Map.union result st.tableData
            , tableFormulas = Map.union (formulaId <$ result)
                st.tableFormulas
            , tableDependencies = Map.unionWith (<>)
                (toDependenciesMap formulaId formulaCells)
                st.tableDependencies
            , formulaCache = Map.insert formulaId
                { formulaText
                , affectedCells
                , startingCell: minimum1 affectedCells
                }
                st.formulaCache
            , formulaState = ValidFormula
            }
          refreshCellsFromDeps cellDeps
        Left err ->
          Logger.error (show err) *>
            modify_ _ { formulaState = InvalidFormula }

handleAction (FormulaKeyPress Tab ev) =
  withPrevent ev $ focusCell =<< gets _.selectedCell

handleAction (FormulaKeyPress (CharKeyCode 'G') ev)
  | ctrlKey ev = withPrevent ev $ focusById formulaCellInputId

handleAction (FormulaKeyPress _ _) =
  modify_ _ { formulaState = UnknownFormula }

handleAction (FocusInFormula _) =
  whenM (not <$> gets _.activeFormula)
    ( modify_ \st -> st
        { activeFormula = true
        , formulaCell = maybe st.selectedCell _.startingCell $
            Map.lookup2 st.selectedCell st.formulaCache st.tableFormulas
        }
    )

handleAction (ClickCell cell ev) = withPrevent ev do
  { selectedCell } <- get
  cellMove ev (OtherCell cell)
  when (cell /= selectedCell) $ modify_ _ { activeInput = false }

handleAction (DoubleClickCell cell ev) = withPrevent ev do
  selectCell (OtherCell cell)
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  focusCellElem selectedCell $ toMaybe' activeInput inputElement

handleAction (FocusInCell cell _) = do
  { tableFormulas } <- get
  let
    formulaState =
      if Map.member cell tableFormulas then
        ValidFormula
      else UnknownFormula
  modify_ _
    { formulaState = formulaState
    , activeFormula = false
    }
  when (formulaState /= ValidFormula) emptyFormulaBox

handleAction (KeyPress x ev) | x `elem` [ ArrowLeft, CharKeyCode 'H' ] =
  cellArrowMove ev PrevColumn

handleAction (KeyPress x ev) | x `elem` [ ArrowRight, CharKeyCode 'L' ] =
  cellArrowMove ev NextColumn

handleAction (KeyPress x ev) | x `elem` [ ArrowUp, CharKeyCode 'K' ] =
  cellArrowMove ev PrevRow

handleAction (KeyPress x ev) | x `elem` [ ArrowDown, CharKeyCode 'J' ] =
  cellArrowMove ev NextRow

handleAction (KeyPress Enter ev)
  | ctrlKey ev = withPrevent ev do
      modify_ _
        { activeInput = false
        , selectionState = NotStartedSelection
        }
      focusById formulaBoxId

handleAction (KeyPress Enter ev) = withPrevent ev do
  { selectedCell, activeInput } <- modify \st -> st
    { activeInput = not st.activeInput }
  focusCellElem selectedCell $ toMaybe' activeInput inputElement

handleAction (KeyPress Tab ev) = selectCell move
  where
  move
    | shiftKey ev = PrevCell
    | otherwise = NextCell

handleAction (KeyPress Space _) = do
  { selectedCell } <- modify _ { activeInput = true }
  focusCellElem selectedCell $ Just inputElement

handleAction (KeyPress Delete _) =
  deleteCells

handleAction (KeyPress Shift ev) = prevent ev

handleAction (KeyPress Control ev) = withPrevent ev $
  modify_ _ { selectionState = InProgressSelection }

handleAction (KeyPress (CharKeyCode 'A') ev)
  | ctrlKey ev = selectAllCells ev

handleAction (KeyPress (CharKeyCode 'C') ev)
  | ctrlKey ev = copyCells ev

handleAction (KeyPress (CharKeyCode 'V') ev)
  | ctrlKey ev = pasteCells ev

handleAction (KeyPress (CharKeyCode 'X') ev)
  | ctrlKey ev = copyCells ev *> deleteCells

handleAction (KeyPress (CharKeyCode 'G') ev)
  | ctrlKey ev = withPrevent ev $ focusById selectedCellInputId

handleAction (KeyPress _ _) =
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

