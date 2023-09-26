module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.CSS.Ids (cellId, formulaBoxId)
import App.Components.Table.Cell (Cell, CellMove, Column, MultiSelection(..), Row(..), SelectionState(..), computeNextSelection, deserializeSelectionValues, getCellFromMove, getTargetCells, parseColumn, parseRow, serializeSelectionValues, showCell)
import App.Components.Table.Formula (FormulaId, getDependencies)
import App.Components.Table.Models (AppState)
import App.Interpreter.Formula (runFormula)
import App.Utils.Dom (class IsEvent, getTarget, scrollByX, selectAllVisibleElements, selectElement, shiftKey, withPrevent)
import App.Utils.Map (updateJust) as Map
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Array as Array
import Data.Map (delete, keys, lookup, union) as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree (Forest)
import Promise.Aff as Promise
import Web.Clipboard (Clipboard, clipboard, readText, writeText)
import Web.DOM (Element)
import Web.DOM.Element (id, scrollWidth)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement
import Web.HTML.Window (navigator)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

cellArrowMove
  :: forall m
   . MonadEffect m
  => MonadState AppState m
  => KeyboardEvent
  -> CellMove
  -> m Unit
cellArrowMove ev move =
  if (shiftKey ev) then
    modify_ \st -> st
      { multiSelection = computeNextSelection st.multiSelection st.selectedCell
          move
          st.columns
          st.rows
      }
  else
    cellMove ev move

cellMove
  :: forall m a
   . MonadEffect m
  => MonadState AppState m
  => IsEvent a
  => a
  -> CellMove
  -> m Unit
cellMove _ move = do
  active <- gets _.activeInput
  when (not active) $ selectCell move

selectAllCells
  :: forall m a
   . MonadEffect m
  => MonadState AppState m
  => IsEvent a
  => a
  -> m Unit
selectAllCells ev = withPrevent ev $
  modify_ _ { multiSelection = AllSelection }

copyCells
  :: forall m a. MonadAff m => MonadState AppState m => IsEvent a => a -> m Unit
copyCells ev = withPrevent ev do
  cellContents <- gets \st -> serializeSelectionValues st.multiSelection
    st.selectedCell
    st.columns
    st.tableData
  modify_ _ { selectionState = CopySelection }
  liftAff $ Promise.toAffE $ writeText cellContents =<< getClipboard

pasteCells
  :: forall m a. MonadAff m => MonadState AppState m => IsEvent a => a -> m Unit
pasteCells ev = withPrevent ev do
  st <- get
  clipContents <- liftAff $ Promise.toAffE $ readText =<< getClipboard
  let
    newValues =
      deserializeSelectionValues st.selectedCell st.columns clipContents
  modify_ _
    { tableData = Map.union
        newValues
        st.tableData
    }
  refreshCells (Map.keys newValues)

deleteCells :: forall m. MonadEffect m => MonadState AppState m => m Unit
deleteCells = do
  st <- get
  let
    cellsToDelete =
      join $ getTargetCells st.multiSelection st.selectedCell st.columns
  modify_ _
    { tableData = foldl (flip Map.delete) st.tableData cellsToDelete }
  refreshCells $ Set.fromFoldable cellsToDelete

getClipboard :: forall m. MonadEffect m => m Clipboard
getClipboard = liftEffect $ clipboard =<< navigator =<< window

selectCell
  :: forall m. MonadEffect m => MonadState AppState m => CellMove -> m Unit
selectCell move = do
  originCell <- gets _.selectedCell
  { selectedCell, columns, rows } <- modify \st -> st
    { activeInput = false
    , multiSelection = NoSelection
    , selectedCell = getCellFromMove move st.columns st.rows st.selectedCell
    }
  visibleCols <- getVisibleCols
  visibleRows <- getVisibleRows
  goToCell visibleCols visibleRows columns rows originCell selectedCell

goToCell
  :: forall m
   . MonadEffect m
  => MonadState AppState m
  => Array Element
  -> Array Element
  -> NonEmptyArray Column
  -> NonEmptyArray Row
  -> Cell
  -> Cell
  -> m Unit
goToCell visibleCols visibleRows allColumns allRows origin target = do
  cols <- parseElems parseColumn visibleCols
  sequence_ $ adjustRows (length visibleRows - 1) target.row <$> maximum allRows
    <*> minimum allRows
  liftEffect $ goToCellHelper cols allColumns origin target visibleCols

goToCellHelper
  :: Array Column
  -> NonEmptyArray Column
  -> Cell
  -> Cell
  -> Array Element
  -> Effect Unit
goToCellHelper cols allColumns origin { column, row } visibleCols

  | last' cols == Just column && last allColumns /= origin.column = do
      width <- traverse scrollWidth $ head' visibleCols
      scrollByX (coalesce width + 1.0) =<< window

  | head' cols == Just column && head allColumns /= origin.column = do
      width <- traverse scrollWidth $ last' visibleCols
      scrollByX (-(coalesce width + 1.0)) =<< window

  | otherwise = focusCell { column, row }

adjustRows
  :: forall m. MonadState AppState m => Int -> Row -> Row -> Row -> m Unit
adjustRows rowRange (Row currentRow) (Row maxRow) (Row minRow)

  | currentRow + 1 > maxRow = modify_ _
      { rows = Row <$> (currentRow - rowRange + 1) .. (currentRow + 1) }

  | currentRow < minRow = modify_ _
      { rows = Row <$> currentRow .. (currentRow + rowRange) }

  | otherwise = pure unit

initialize :: forall m. MonadState AppState m => MonadEffect m => m Unit
initialize = do
  { selectedCell, rows } <- get
  let Row (firstRow) = head rows
  visibleRows <- parseElems parseRow =<< getVisibleRows
  modify_ _
    { rows = Row <$> firstRow .. (firstRow + length visibleRows - 2) }
  focusCell selectedCell

refreshCells :: forall m. MonadState AppState m => Set Cell -> m Unit
refreshCells affectedCells = do
  st <- get
  traverse_ refreshCellsFromDeps $ cellDeps st
  where
  cellDeps st = join $ hush <<< (\x -> getDependencies st x Set.empty) <$>
    NonEmptySet.fromSet affectedCells

refreshCellsFromDeps
  :: forall m. MonadState AppState m => Forest FormulaId -> m Unit
refreshCellsFromDeps cellDeps =
  traverse_ (traverse_ applyFormula) cellDeps

applyFormula :: forall m. MonadState AppState m => FormulaId -> m Unit
applyFormula formulaId = do
  st <- get
  let
    { formulaText, startingCell } =
      unsafeFromJust $ Map.lookup formulaId st.formulaCache
    eitherResult = runFormula st startingCell formulaText
  case eitherResult of
    Right { result, affectedCells } ->
      modify_ _
        { tableData = Map.union result st.tableData
        , formulaCache = Map.updateJust (_ { affectedCells = affectedCells })
            formulaId
            st.formulaCache
        }
    Left _ ->
      pure unit

parseElems
  :: forall m a
   . MonadEffect m
  => (String -> Maybe a)
  -> Array Element
  -> m (Array a)
parseElems f elems = liftEffect
  (Array.catMaybes <$> traverse ((f <$> _) <<< id) elems)

getVisibleCols :: forall m. MonadEffect m => m (Array Element)
getVisibleCols = selectAllVisibleElements $ QuerySelector "th.column-header"

getVisibleRows :: forall m. MonadEffect m => m (Array Element)
getVisibleRows = selectAllVisibleElements $ QuerySelector "th.row-header"

focusCell :: forall m. MonadEffect m => Cell -> m Unit
focusCell = (_ `focusCellElem` Nothing)

focusCellElem :: forall m. MonadEffect m => Cell -> Maybe String -> m Unit
focusCellElem cell subElem = actOnCellElem cell focus subElem

focusById :: forall m. MonadEffect m => String -> m Unit
focusById = (_ `actOnElemById` focus)

actOnCellElem
  :: forall m
   . MonadEffect m
  => Cell
  -> (HTMLElement -> Effect Unit)
  -> Maybe String
  -> m Unit
actOnCellElem cell action subElem =
  actOnElemById (cellId <> showCell cell <> foldMap (" " <> _) subElem) action

actOnElemById
  :: forall m
   . MonadEffect m
  => String
  -> (HTMLElement -> Effect Unit)
  -> m Unit
actOnElemById id action = do
  element <- selectElement $ QuerySelector ("#" <> id)
  liftEffect $ traverse_ action element

getFormulaBoxContents
  :: forall m ev. MonadEffect m => IsEvent ev => ev -> m String
getFormulaBoxContents ev =
  liftEffect $ fold <$>
    ( traverse HTMLTextAreaElement.value
        $ HTMLTextAreaElement.fromEventTarget
        =<< getTarget ev
    )

emptyFormulaBox :: forall m. Bind m => MonadEffect m => m Unit
emptyFormulaBox = do
  formulaBox <- join <<< map HTMLTextAreaElement.fromHTMLElement <$>
    selectElement (QuerySelector $ "#" <> formulaBoxId)
  liftEffect $ traverse_ (HTMLTextAreaElement.setValue "") formulaBox
