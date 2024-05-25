module App.Components.Spreadsheet.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.AppStore (Store, StoreAction, cleanupStore)
import App.CSS.Ids (ElementType, cellId)
import App.Components.Editor (EditorSlot, _editor)
import App.Components.Editor.Models (EditorQuery(..))
import App.Components.Spreadsheet.Cell (Cell, CellMove, Column, Row(..), columnParser, rowParser)
import App.Components.Spreadsheet.Formula (Formula, FormulaId, FormulaState(..), getDependencies, newFormulaId, toDependenciesMap)
import App.Components.Spreadsheet.Models (SpreadsheetAction(..), SpreadsheetState)
import App.Components.Spreadsheet.Selection (MultiSelection(..), SelectionState(..), computeNextSelection, deserializeSelectionValues, getCellFromMove, getTargetCells, serializeSelectionValues)
import App.Interpreter.Formula (parseAndRunFormula, runFormula)
import App.Utils.Dom (actOnElementById, getClipboard, getDocumentElement, getElemWidth, parseElements, selectAllVisibleElements, withPrevent)
import App.Utils.Event (class IsEvent, shiftKey)
import App.Utils.HashMap (bulkDelete, lookup2, updateJust) as HashMap
import Bookhound.Parser (runParser)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.HashMap (insert, keys, lookup, union, unionWith) as HashMap
import Data.Newtype as Newtype
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String.Utils (NormalizationForm(..))
import Data.String.Utils as String
import Data.Tree (Forest)
import Effect.Class.Console as Logger
import Halogen (HalogenM, subscribe)
import Halogen.Query (tell)
import Halogen.Query.Event (eventListener)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Promise.Aff as Promise
import Web.Clipboard (readText, writeText)
import Web.DOM (Element)
import Web.DOM.Element (scrollLeft, setScrollLeft)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (EventType(..))
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

cellArrowMove
  :: forall m
   . MonadEffect m
  => MonadState SpreadsheetState m
  => KeyboardEvent
  -> CellMove
  -> m Unit
cellArrowMove ev move
  | shiftKey ev =
      modify_ \st -> st
        { multiSelection = computeNextSelection st.multiSelection
            st.selectedCell
            move
        }
  | otherwise =
      cellMove ev move

cellMove
  :: forall m a
   . MonadEffect m
  => MonadState SpreadsheetState m
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
  => MonadState SpreadsheetState m
  => IsEvent a
  => a
  -> m Unit
selectAllCells ev = withPrevent ev $
  modify_ _ { multiSelection = AllSelection }

copyCells
  :: forall m a
   . MonadAff m
  => MonadState SpreadsheetState m
  => IsEvent a
  => a
  -> m Unit
copyCells ev = withPrevent ev do
  cellContents <- gets \st ->
    serializeSelectionValues st.multiSelection st.selectedCell st.tableData
  modify_ _ { selectionState = CopySelection }
  liftAff $ Promise.toAffE $ writeText cellContents =<< getClipboard

pasteCells
  :: forall m a
   . MonadAff m
  => MonadState SpreadsheetState m
  => MonadStore StoreAction Store m
  => IsEvent a
  => a
  -> m Unit
pasteCells ev = withPrevent ev do
  { selectedCell } <- get
  clipContents <- liftAff $ Promise.toAffE $ readText =<< getClipboard
  let
    newValues =
      deserializeSelectionValues selectedCell clipContents
  updateStore \store -> store
    { tableData = HashMap.union
        newValues
        store.tableData
    }
  refreshCells (Set.fromFoldable $ HashMap.keys newValues)

deleteCells
  :: forall r a o m
   . MonadStore StoreAction Store m
  => HalogenM SpreadsheetState a (editor :: EditorSlot | r) o m Unit
deleteCells = do
  { multiSelection, selectedCell } <- get
  let
    cellsToDelete =
      join $ getTargetCells multiSelection selectedCell
  updateStore \store -> store
    { tableData = HashMap.bulkDelete cellsToDelete store.tableData
    , tableFormulas = HashMap.bulkDelete cellsToDelete store.tableFormulas
    }
  updateStore cleanupStore
  modify_ _ { formulaState = UnknownFormula }
  refreshCells $ Set.fromFoldable cellsToDelete
  tell _editor unit $ UpdateEditorContent mempty

selectCell
  :: forall m
   . MonadEffect m
  => MonadState SpreadsheetState m
  => CellMove
  -> m Unit
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
  => MonadState SpreadsheetState m
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

  | Array.last cols == Just column && origin.column /= top
  , Just element <- Array.head visibleCols = do
      scrollCellRight element

  | Array.head cols == Just column && origin.column /= bottom
  , Just element <- Array.head visibleCols = do
      scrollCellLeft element

  | otherwise = focusCell { column, row }

adjustRows
  :: forall m
   . MonadState SpreadsheetState m
  => Int
  -> Row
  -> Row
  -> Row
  -> m Unit
adjustRows rowRange currentRow maxRow minRow

  | inc currentRow > maxRow =
      modify_ _
        { rows = clampBounded (inc currentRow - wrap rowRange)
            .. clampBounded (inc currentRow)
        }

  | currentRow < minRow = modify_ _
      { rows = clampBounded currentRow
          .. clampBounded (currentRow + wrap rowRange)
      }

  | otherwise = pure unit

refreshCells
  :: forall m
   . MonadState SpreadsheetState m
  => MonadStore StoreAction Store m
  => Set Cell
  -> m Unit
refreshCells affectedCells = do
  st <- get
  traverse_ refreshCellsFromDeps $ cellDeps st
  where
  cellDeps st = join $ hush <<< (\x -> getDependencies st x Set.empty) <$>
    NonEmptySet.fromSet affectedCells

refreshCellsFromDeps
  :: forall m
   . MonadState SpreadsheetState m
  => MonadStore StoreAction Store m
  => Forest FormulaId
  -> m Unit
refreshCellsFromDeps cellDeps =
  traverse_ (traverse_ applyFormula) cellDeps

insertFormula
  :: forall m
   . MonadEffect m
  => MonadState SpreadsheetState m
  => MonadStore StoreAction Store m
  => String
  -> m Unit
insertFormula editorText = do
  let formulaText = String.normalize' NFKD editorText
  { formulaCell, selectedCell } <- get
  store <- getStore
  case parseAndRunFormula store formulaCell formulaText of
    Right
      ( formulaBody /\
          { result, affectedCells, formulaCells, cellDeps }
      ) -> do
      let formulaId = newFormulaId $ HashMap.keys store.formulaCache
      updateStore _
        { tableData = HashMap.union result store.tableData
        , tableFormulas = HashMap.union (formulaId <$ result)
            store.tableFormulas
        , tableDependencies = HashMap.unionWith (<>)
            (toDependenciesMap formulaId formulaCells)
            store.tableDependencies
        , formulaCache = HashMap.insert formulaId
            { formulaBody
            , affectedCells
            , startingCell: minimum1 affectedCells
            }
            store.formulaCache
        }
      modify_ _ { formulaState = ValidFormula }
      refreshCellsFromDeps cellDeps
      focusCell selectedCell
    Left err -> do
      Logger.error (show err)
      modify_ _ { formulaState = InvalidFormula }

applyFormula
  :: forall m
   . MonadState SpreadsheetState m
  => MonadStore StoreAction Store m
  => FormulaId
  -> m Unit
applyFormula formulaId = do
  st <- get
  store <- getStore
  let
    { formulaBody, startingCell } =
      unsafeFromJust $ HashMap.lookup formulaId st.formulaCache
    eitherResult = runFormula store startingCell formulaBody
  case eitherResult of
    Right { result, affectedCells } ->
      modify_ _
        { tableData = HashMap.union result store.tableData
        , formulaCache = HashMap.updateJust
            (_ { affectedCells = affectedCells })
            formulaId
            st.formulaCache
        }
    Left _ ->
      pure unit

lookupFormula
  :: forall m. MonadState SpreadsheetState m => Cell -> m (Maybe Formula)
lookupFormula cell = do
  { formulaCache, tableFormulas } <- get
  pure $ HashMap.lookup2 cell tableFormulas formulaCache

setRows :: forall m. MonadEffect m => MonadState SpreadsheetState m => m Unit
setRows = do
  { selectedCell, rows } <- get
  let Row (firstRow) = NonEmptyArray.head rows
  visibleRows <- parseElements parseRow =<< getVisibleRows
  modify_ _
    { rows = Row <$> firstRow .. (firstRow + length visibleRows + 2) }
  focusCell selectedCell
  where
  parseRow = hush <<< runParser rowParser

subscribeWindowResize
  :: forall slots o m
   . MonadEffect m
  => HalogenM SpreadsheetState SpreadsheetAction slots o m Unit
subscribeWindowResize = do
  window' <- liftEffect window
  void $ subscribe $ eventListener
    (EventType "resize")
    (Window.toEventTarget window')
    (const $ Just ResizeWindow)

getVisibleCols :: forall m. MonadEffect m => m (Array Element)
getVisibleCols = selectAllVisibleElements $ QuerySelector "th.column-header"

getVisibleRows :: forall m. MonadEffect m => m (Array Element)
getVisibleRows = selectAllVisibleElements $ QuerySelector "th.row-header"

focusCellElem :: forall m. MonadEffect m => Cell -> Maybe ElementType -> m Unit
focusCellElem cell subElem = actOnCellElem cell focus subElem

focusCell :: forall m. MonadEffect m => Cell -> m Unit
focusCell = flip focusCellElem Nothing

scrollCellRight :: Element -> Effect Unit
scrollCellRight = scrollCell (+)

scrollCellLeft :: Element -> Effect Unit
scrollCellLeft = scrollCell (-)

scrollCell :: (Number -> Number -> Number) -> Element -> Effect Unit
scrollCell f element = do
  traverse_ go =<< getDocumentElement
  where
  go table = liftEffect do
    scroll <- scrollLeft table
    width <- getElemWidth element
    setScrollLeft (f scroll width) table

actOnCellElem
  :: forall m
   . MonadEffect m
  => Cell
  -> (HTMLElement -> Effect Unit)
  -> Maybe ElementType
  -> m Unit
actOnCellElem cell action subElem =
  actOnElementById selector action
  where
  selector = Newtype.modify (_ <> subElemSelector) $ cellId cell
  subElemSelector = foldMap ((" " <> _) <<< show) subElem
