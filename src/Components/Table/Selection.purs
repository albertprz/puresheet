module App.Components.Table.Selection where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellMove(..), CellValue, Column, Row, allColumns, allRows, getCell, getColumnCell, getRowCell, nextRowCell, parseCellValue, prevColumnCell)
import Data.HashMap as HashMap
import Data.String.Pattern (Pattern(..))

isColumnSelected :: Cell -> MultiSelection -> Column -> Boolean
isColumnSelected selectedCell multiSelection column =
  selectedCell.column == column ||
    isColumnInSelection multiSelection column

isRowSelected :: Cell -> MultiSelection -> Row -> Boolean
isRowSelected selectedCell multiSelection row =
  selectedCell.row == row ||
    isRowInSelection multiSelection row

isCellAtRightSelection :: MultiSelection -> Cell -> Boolean
isCellAtRightSelection selection cell = (not $ isCellInSelection selection cell)
  && (isCellInSelection selection $ prevColumnCell cell)

isCellAboveSelection :: MultiSelection -> Cell -> Boolean
isCellAboveSelection selection cell = (not $ isCellInSelection selection cell)
  && (isCellInSelection selection $ nextRowCell cell)

isCellAtLeftSelection :: MultiSelection -> Cell -> Boolean
isCellAtLeftSelection selection cell = (isCellInSelection selection cell) &&
  (not $ isCellInSelection selection $ prevColumnCell cell)

isCellBelowSelection :: MultiSelection -> Cell -> Boolean
isCellBelowSelection selection cell = (isCellInSelection selection cell) &&
  (not $ isCellInSelection selection $ nextRowCell cell)

isColumnInSelection :: MultiSelection -> Column -> Boolean
isColumnInSelection NoSelection _ = false
isColumnInSelection AllSelection _ = true
isColumnInSelection (RowsSelection _ _) _ = true
isColumnInSelection (ColumnsSelection origin target) col =
  inRange origin target col
isColumnInSelection (CellsSelection origin target) col =
  inRange origin.column target.column col

isRowInSelection :: MultiSelection -> Row -> Boolean
isRowInSelection NoSelection _ = false
isRowInSelection AllSelection _ = true
isRowInSelection (ColumnsSelection _ _) _ = true
isRowInSelection (RowsSelection origin target) row =
  inRange origin target row
isRowInSelection (CellsSelection origin target) row =
  inRange origin.row target.row row

isCellInSelection :: MultiSelection -> Cell -> Boolean
isCellInSelection NoSelection _ = false
isCellInSelection AllSelection _ = true
isCellInSelection (ColumnsSelection origin target) cell =
  inRange origin target cell.column
isCellInSelection (RowsSelection origin target) cell =
  inRange origin target cell.row
isCellInSelection (CellsSelection origin target) { column, row } =
  inRange origin.row target.row row &&
    inRange origin.column target.column column

getCellFromMove :: CellMove -> Cell -> Cell
getCellFromMove move cell =
  fromMaybe cell $ interpretCellMove move cell

computeNextSelection
  :: MultiSelection
  -> Cell
  -> CellMove
  -> MultiSelection
computeNextSelection (CellsSelection origin target) _ move =
  CellsSelection origin
    $ getCellFromMove move target
computeNextSelection (ColumnsSelection origin target) _ move =
  ColumnsSelection origin
    (fromMaybe origin $ interpretColumnMove move target)
computeNextSelection (RowsSelection origin target) _ move =
  RowsSelection origin (fromMaybe origin $ interpretRowMove move target)
computeNextSelection NoSelection selectedCell move =
  CellsSelection selectedCell
    $ getCellFromMove move selectedCell
computeNextSelection AllSelection _ _ = AllSelection

interpretCellMove :: CellMove -> Cell -> Maybe Cell
interpretCellMove = case _ of
  NextRow -> getRowCell inc
  PrevRow -> getRowCell dec
  NextColumn -> getColumnCell inc
  PrevColumn -> getColumnCell dec
  NextCell -> getCell inc
  PrevCell -> getCell dec
  OtherColumn column -> \cell -> Just $ cell { column = column }
  OtherRow row -> \cell -> Just $ cell { row = row }
  OtherCell cell -> const $ Just cell

interpretColumnMove
  :: CellMove -> Column -> Maybe Column
interpretColumnMove = case _ of
  NextColumn -> getInBoundedRange <<< inc
  PrevColumn -> getInBoundedRange <<< dec
  _ -> const Nothing

interpretRowMove :: CellMove -> Row -> Maybe Row
interpretRowMove = case _ of
  NextRow -> getInBoundedRange <<< inc
  PrevRow -> getInBoundedRange <<< dec
  _ -> const Nothing

serializeSelectionValues
  :: MultiSelection
  -> Cell
  -> HashMap Cell CellValue
  -> String
serializeSelectionValues selection selectedCell tableData =
  intercalate newline
    $ intercalate tab
    <$> (foldMap show <<< flip HashMap.lookup tableData)
    <$$> getTargetCells selection selectedCell

deserializeSelectionValues
  :: Cell -> String -> HashMap Cell CellValue
deserializeSelectionValues selectedCell str = HashMap.fromArray
  do
    rowValues /\ row <- zip' values (selectedCell.row .. top)
    value /\ column <- zip' rowValues (selectedCell.column .. top)
    pure $ { row, column } /\ parseCellValue value
  where
  values = split (Pattern tab) <$> split (Pattern newline) str

getTargetCells
  :: MultiSelection
  -> Cell
  -> (NonEmptyArray (NonEmptyArray Cell))
getTargetCells selection selectedCell =
  fromMaybe (singleton $ singleton selectedCell)
    (getSelectionCells selection)

getSelectionCells
  :: MultiSelection
  -> Maybe (NonEmptyArray (NonEmptyArray Cell))
getSelectionCells selection = do
  columnBounds /\ rowBounds <- getSelectionBounds selection
  pure do
    row <- rowBounds
    pure do
      column <- columnBounds
      pure $ { column, row }

isCellsSelection :: MultiSelection -> Boolean
isCellsSelection (CellsSelection _ _) = true
isCellsSelection NoSelection = true
isCellsSelection _ = false

isColumnsSelection :: MultiSelection -> Boolean
isColumnsSelection (ColumnsSelection _ _) = true
isColumnsSelection NoSelection = true
isColumnsSelection _ = false

isRowsSelection :: MultiSelection -> Boolean
isRowsSelection (RowsSelection _ _) = true
isRowsSelection NoSelection = true
isRowsSelection _ = false

getSelectionBounds
  :: MultiSelection
  -> Maybe (NonEmptyArray Column /\ NonEmptyArray Row)
getSelectionBounds NoSelection = Nothing
getSelectionBounds AllSelection =
  Just $ allColumns /\ allRows
getSelectionBounds (ColumnsSelection origin target) =
  Just $ sort (origin .. target) /\ allRows
getSelectionBounds (RowsSelection origin target) =
  Just $ allColumns /\ sort (origin .. target)
getSelectionBounds
  ( CellsSelection { column: column, row: row }
      { column: column', row: row' }
  ) = Just $ sort (column .. column') /\ sort (row .. row')

data MultiSelection
  = RowsSelection Row Row
  | ColumnsSelection Column Column
  | CellsSelection Cell Cell
  | AllSelection
  | NoSelection

data SelectionState
  = InProgressSelection
  | NotStartedSelection
  | CopySelection

derive instance Eq SelectionState
