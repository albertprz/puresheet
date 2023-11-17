module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellMove, Column, Row(..), columnParser, rowParser)
import App.Components.Table.Formula (Formula, FormulaId, FormulaState(..), getDependencies, newFormulaId, toDependenciesMap)
import App.Components.Table.Models (Action(..), AppState)
import App.Components.Table.Selection (MultiSelection(..), SelectionState(..), computeNextSelection, deserializeSelectionValues, getCellFromMove, getTargetCells, serializeSelectionValues)
import App.Interpreter.Formula (runFormula)
import App.Interpreter.Module (reloadModule)
import App.Utils.Dom (class IsEvent, emptyFormulaBox, focusCell, getClipboard, getFormulaBoxContents, getVisibleCols, getVisibleRows, parseElements, scrollCellLeft, scrollCellRight, shiftKey, withPrevent)
import App.Utils.HashMap (bulkDelete, lookup2, updateJust) as HashMap
import Bookhound.Parser (runParser)
import Data.HashMap (insert, keys, lookup, union, unionWith) as HashMap
import Data.List.NonEmpty (NonEmptyList)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String.Utils (NormalizationForm(..))
import Data.String.Utils as String
import Data.Tree (Forest)
import Effect.Class.Console as Logger
import Foreign (ForeignError, readString, unsafeToForeign)
import Foreign.Index ((!))
import Halogen (HalogenM, subscribe')
import Halogen.Query.Event (eventListener)
import Promise.Aff as Promise
import Web.Clipboard (readText, writeText)
import Web.DOM (Element)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
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
      deserializeSelectionValues st.selectedCell clipContents
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
      join $ getTargetCells st.multiSelection st.selectedCell
  modify_ _
    { tableData = HashMap.bulkDelete cellsToDelete st.tableData
    , tableFormulas = HashMap.bulkDelete cellsToDelete st.tableFormulas
    , formulaState = UnknownFormula
    }
  refreshCells $ Set.fromFoldable cellsToDelete
  emptyFormulaBox

selectCell
  :: forall m. MonadEffect m => MonadState AppState m => CellMove -> m Unit
selectCell move = do
  target <- goToCell move
  modify_ _
    { activeInput = false
    , multiSelection = NoSelection
    , selectedCell = target
    }

goToCell
  :: forall m
   . MonadEffect m
  => MonadState AppState m
  => CellMove
  -> m Cell
goToCell move = do
  { rows, selectedCell: origin } <- get
  let target = getCellFromMove move origin
  visibleCols <- getVisibleCols
  visibleRows <- getVisibleRows
  cols <- parseElements parseColumns visibleCols
  adjustRows (length visibleRows - 1) target.row
    (maximum1 rows)
    (minimum1 rows)
  liftEffect $ goToCellHelper cols origin target visibleCols
  pure target
  where
  parseColumns = hush <<< runParser columnParser

goToCellHelper
  :: Array Column
  -> Cell
  -> Cell
  -> Array Element
  -> Effect Unit
goToCellHelper cols origin { column, row } visibleCols

  | last' cols == Just column && origin.column /= top
  , Just element <- head' visibleCols = do
      scrollCellRight element

  | head' cols == Just column && origin.column /= bottom
  , Just element <- head' visibleCols = do
      scrollCellLeft element

  | otherwise = focusCell { column, row }

adjustRows
  :: forall m. MonadState AppState m => Int -> Row -> Row -> Row -> m Unit
adjustRows rowRange currentRow maxRow minRow

  | inc currentRow > maxRow =
      modify_ _
        { rows = clampBounded (currentRow - wrap (rowRange))
            .. clampBounded (inc currentRow)
        }

  | currentRow < minRow = modify_ _
      { rows = clampBounded (currentRow)
          .. clampBounded (currentRow + wrap (rowRange))
      }

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
  formulaText <- String.normalize' NFKD <$> getFormulaBoxContents
  st <- get
  case runFormula st st.formulaCell formulaText of
    Right { result, affectedCells, formulaCells, cellDeps } -> do
      let formulaId = newFormulaId $ HashMap.keys st.formulaCache
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

lookupFormula :: forall m. MonadState AppState m => Cell -> m (Maybe Formula)
lookupFormula cell = do
  { formulaCache, tableFormulas } <- get
  pure $ HashMap.lookup2 cell tableFormulas formulaCache

setRows :: forall m. MonadEffect m => MonadState AppState m => m Unit
setRows = do
  { selectedCell, rows } <- get
  let Row (firstRow) = head rows
  visibleRows <- parseElements parseRow =<< getVisibleRows
  modify_ _
    { rows = Row <$> firstRow .. (firstRow + length visibleRows + 2) }
  focusCell selectedCell
  where
  parseRow = hush <<< runParser rowParser

getPrelude
  :: forall m. MonadEffect m => m (Either (NonEmptyList ForeignError) String)
getPrelude = runExceptT
  $ readString
  =<< (_ ! "prelude")
  =<< unsafeToForeign
  <$> liftEffect window

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

subscribeSelectionChange
  :: forall slots o m
   . MonadEffect m
  => HalogenM AppState Action slots o m Unit
subscribeSelectionChange = do
  doc <- liftEffect $ Window.document =<< window
  subscribe' \_ -> eventListener
    (EventType "selectionchange")
    (HTMLDocument.toEventTarget doc)
    (const $ Just SelectionChange)

subscribeWindowResize
  :: forall slots o m
   . MonadEffect m
  => HalogenM AppState Action slots o m Unit
subscribeWindowResize = do
  window' <- liftEffect window
  subscribe' \_ -> eventListener
    (EventType "resize")
    (Window.toEventTarget window')
    (const $ Just ResizeWindow)
