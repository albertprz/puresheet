module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell
  ( Cell
  , CellMove
  , Column
  , MultiSelection(..)
  , Row(..)
  , SelectionState(..)
  , computeNextSelection
  , deserializeSelectionValues
  , getCellFromMove
  , getTargetCells
  , parseColumn
  , parseRow
  , serializeSelectionValues
  , showCell
  )
import App.Components.Table.Models (State)
import App.Utils.Dom
  ( class IsEvent
  , scrollByX
  , selectAllVisibleElements
  , selectElement
  , shiftKey
  , withPrevent
  )
import Data.Array as Array
import Data.Map as Map
import Promise.Aff as Promise
import Web.Clipboard (Clipboard, clipboard, readText, writeText)
import Web.DOM (Element)
import Web.DOM.Element (id, scrollWidth)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Window (navigator)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

cellArrowMove
  :: forall m
   . MonadEffect m
  => MonadState State m
  => KeyboardEvent
  -> CellMove
  -> m Unit
cellArrowMove ev move = withPrevent ev $
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
  => MonadState State m
  => IsEvent a
  => a
  -> CellMove
  -> m Unit
cellMove ev move = withPrevent ev do
  active <- gets _.activeInput
  when (not active) $ selectCell move

selectAllCells
  :: forall m a. MonadEffect m => MonadState State m => IsEvent a => a -> m Unit
selectAllCells ev = withPrevent ev $
  modify_ _ { multiSelection = AllSelection }

copyCells
  :: forall m a. MonadAff m => MonadState State m => IsEvent a => a -> m Unit
copyCells ev = withPrevent ev do
  cellContents <- gets \st -> serializeSelectionValues st.multiSelection
    st.selectedCell
    st.columns
    st.tableData
  modify_ _ { selectionState = CopySelection }
  liftAff $ Promise.toAffE $ writeText cellContents =<< getClipboard

pasteCells
  :: forall m a. MonadAff m => MonadState State m => IsEvent a => a -> m Unit
pasteCells ev = withPrevent ev do
  clipContents <- liftAff $ Promise.toAffE $ readText =<< getClipboard
  modify_ \st -> st
    { tableData = Map.union
        (deserializeSelectionValues st.selectedCell st.columns clipContents)
        st.tableData
    }

deleteCells
  :: forall m a. MonadEffect m => MonadState State m => IsEvent a => a -> m Unit
deleteCells ev = withPrevent ev $
  modify_ \st -> st
    { tableData = foldl (flip Map.delete) st.tableData $ join $ getTargetCells
        st.multiSelection
        st.selectedCell
        st.columns
    }

getClipboard :: forall m. MonadEffect m => m Clipboard
getClipboard = liftEffect $ clipboard =<< navigator =<< window

selectCell
  :: forall m. MonadEffect m => MonadState State m => CellMove -> m Unit
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
  => MonadState State m
  => Array Element
  -> Array Element
  -> NonEmptyArray Column
  -> NonEmptyArray Row
  -> Cell
  -> Cell
  -> m Unit
goToCell visibleCols visibleRows allColumns allRows origin target = do
  cols <- parseElems parseColumn visibleCols
  rows <- parseElems parseRow visibleRows
  sequence_ $ adjustRows (length visibleRows - 1) target.row <$> maximum allRows
    <*> minimum allRows
  liftEffect $ goToCellHelper cols rows allColumns origin target visibleCols

goToCellHelper
  :: Array Column
  -> Array Row
  -> NonEmptyArray Column
  -> Cell
  -> Cell
  -> Array Element
  -> Effect Unit
goToCellHelper cols rows allColumns origin { column, row } visibleCols

  | last' cols == Just column && last allColumns /= origin.column = do
      width <- traverse scrollWidth $ head' visibleCols
      scrollByX (coalesce width + 1.0) =<< window

  | head' cols == Just column && head allColumns /= origin.column = do
      width <- traverse scrollWidth $ last' visibleCols
      scrollByX (-(coalesce width + 1.0)) =<< window

  | not (elem row rows && elem column cols) =
      actOnCell { column, row } focus Nothing

  | otherwise = pure unit

adjustRows :: forall m. MonadState State m => Int -> Row -> Row -> Row -> m Unit
adjustRows rowRange (Row currentRow) (Row maxRow) (Row minRow)

  | currentRow + 1 > maxRow = modify_ _
      { rows = Row <$> (currentRow - rowRange + 1) .. (currentRow + 1) }

  | currentRow < minRow = modify_ _
      { rows = Row <$> currentRow .. (currentRow + rowRange) }

  | otherwise = pure unit

initialize :: forall m. MonadState State m => MonadEffect m => m Unit
initialize = do
  { selectedCell, rows } <- get
  let Row (firstRow) = head rows
  visibleRows <- parseElems parseRow =<< getVisibleRows
  modify_ _
    { rows = Row <$> firstRow .. (firstRow + length visibleRows - 2) }
  actOnCell selectedCell focus Nothing

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

actOnCell
  :: forall m
   . MonadEffect m
  => Cell
  -> (HTMLElement -> Effect Unit)
  -> Maybe String
  -> m Unit
actOnCell cell action subElem = do
  element <- selectElement $ QuerySelector $ "td#"
    <> showCell cell
    <> foldMap (" " <> _) subElem
  liftEffect $ traverse_ action element
