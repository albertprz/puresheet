module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellMove, Column, Row(..), columnParser, rowParser)
import App.Components.Table.Formula (FormulaId, FormulaState(..), getDependencies, newFormulaId, toDependenciesMap)
import App.Components.Table.Models (AppState)
import App.Components.Table.Selection (MultiSelection(..), SelectionState(..), computeNextSelection, deserializeSelectionValues, getCellFromMove, getTargetCells, serializeSelectionValues)
import App.Interpreter.Formula (runFormula)
import App.Interpreter.Module (reloadModule)
import App.Utils.Dom (class IsEvent, emptyFormulaBox, focusCell, getClipboard, getFormulaBoxContents, getVisibleCols, getVisibleRows, parseElements, scrollByX, shiftKey, withPrevent)
import App.Utils.HashMap (updateJust) as HashMap
import Bookhound.Parser (runParser)
import Data.HashMap (delete, insert, keys, lookup, union, unionWith) as HashMap
import Data.List.NonEmpty (NonEmptyList)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree (Forest)
import Effect.Class.Console as Logger
import Foreign (ForeignError, readString, unsafeToForeign)
import Foreign.Index ((!))
import Promise.Aff as Promise
import Web.Clipboard (readText, writeText)
import Web.DOM (Element)
import Web.DOM.Element (scrollWidth)
import Web.HTML (window)
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
    { tableData = HashMap.union
        newValues
        st.tableData
    }
  refreshCells (Set.fromFoldable $ HashMap.keys newValues)

deleteCells :: forall m. MonadEffect m => MonadState AppState m => m Unit
deleteCells = do
  st <- get
  let
    cellsToDelete =
      join $ getTargetCells st.multiSelection st.selectedCell st.columns
  modify_ _
    { tableData = foldl (flip HashMap.delete) st.tableData cellsToDelete }
  refreshCells $ Set.fromFoldable cellsToDelete

getPrelude
  :: forall m. MonadEffect m => m (Either (NonEmptyList ForeignError) String)
getPrelude = runExceptT
  $ readString
  =<< (_ ! "prelude")
  =<< unsafeToForeign
  <$> liftEffect window

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
  cols <- parseElements parseColumn visibleCols
  sequence_ $ adjustRows (length visibleRows - 1) target.row <$> maximum allRows
    <*> minimum allRows
  liftEffect $ goToCellHelper cols allColumns origin target visibleCols
  where
  parseColumn = hush <<< runParser columnParser

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

  | inc currentRow > maxRow = modify_ _
      { rows = Row <$> (currentRow - inc rowRange) .. (inc currentRow) }

  | currentRow < minRow = modify_ _
      { rows = Row <$> currentRow .. (currentRow + rowRange) }

  | otherwise = pure unit

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

insertFormula :: forall m. MonadEffect m => MonadState AppState m => m Unit
insertFormula = do
  formulaText <- getFormulaBoxContents
  st <- get
  case runFormula st st.formulaCell formulaText of
    Right { result, affectedCells, formulaCells, cellDeps } -> do
      let formulaId = newFormulaId $ HashMap.keys st.formulaCache
      emptyFormulaBox
      modify_ _
        { tableData = HashMap.union result st.tableData
        , tableFormulas = HashMap.union (formulaId <$ result)
            st.tableFormulas
        , tableDependencies = HashMap.unionWith (<>)
            (toDependenciesMap formulaId formulaCells)
            st.tableDependencies
        , formulaCache = HashMap.insert formulaId
            { formulaText
            , affectedCells
            , startingCell: minimum1 affectedCells
            }
            st.formulaCache
        , formulaState = ValidFormula
        }
      refreshCellsFromDeps cellDeps
      focusCell st.selectedCell
    Left err ->
      Logger.error (show err) *>
        modify_ _ { formulaState = InvalidFormula }

applyFormula :: forall m. MonadState AppState m => FormulaId -> m Unit
applyFormula formulaId = do
  st <- get
  let
    { formulaText, startingCell } =
      unsafeFromJust $ HashMap.lookup formulaId st.formulaCache
    eitherResult = runFormula st startingCell formulaText
  case eitherResult of
    Right { result, affectedCells } ->
      modify_ _
        { tableData = HashMap.union result st.tableData
        , formulaCache = HashMap.updateJust
            (_ { affectedCells = affectedCells })
            formulaId
            st.formulaCache
        }
    Left _ ->
      pure unit

loadPrelude :: forall m. MonadEffect m => MonadState AppState m => m Unit
loadPrelude = do
  tryLoad <- sequence <$> (traverse reloadModule =<< getPrelude)
  case tryLoad of
    Left err ->
      Logger.error
        ( "Prelude load error \n" <> "Parse Error: " <>
            show err
        )
    Right _ -> pure unit

setRows :: forall m. MonadEffect m => MonadState AppState m => m Unit
setRows = do
  { selectedCell, rows } <- get
  let Row (firstRow) = head rows
  visibleRows <- parseElements parseRow =<< getVisibleRows
  modify_ _
    { rows = Row <$> firstRow .. (firstRow + length visibleRows - 2) }
  focusCell selectedCell
  where
  parseRow = hush <<< runParser rowParser
