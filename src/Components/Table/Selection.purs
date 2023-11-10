module App.Components.Table.Selection where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellMove(..), CellValue, Column, Row, getCell, getColumnCell, getRowCell, maxRow, maxRowBounds, nextRowCell, parseCellValue, prevColumnCell)
import Data.Array as Array
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
  Array.elem col (origin .. target)
isColumnInSelection (CellsSelection origin target) col =
  inRange origin.column target.column col

isRowInSelection :: MultiSelection -> Row -> Boolean
isRowInSelection NoSelection _ = false
isRowInSelection AllSelection _ = true
isRowInSelection (ColumnsSelection _ _) _ = true
isRowInSelection (RowsSelection origin target) row =
  Array.elem row (origin .. target)
isRowInSelection (CellsSelection origin target) row =
  inRange origin.row target.row row

isCellInSelection :: MultiSelection -> Cell -> Boolean
isCellInSelection NoSelection _ = false
isCellInSelection AllSelection _ = true
isCellInSelection (ColumnsSelection origin target) cell =
  Array.elem cell.column (origin .. target)
isCellInSelection (RowsSelection origin target) cell =
  Array.elem cell.row (origin .. target)
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
  NextColumn -> getElemSat <<< inc
  PrevColumn -> getElemSat <<< dec
  _ -> const Nothing

interpretRowMove :: CellMove -> Row -> Maybe Row
interpretRowMove = case _ of
  NextRow -> getElemSat <<< inc
  PrevRow -> getElemSat <<< dec
  _ -> const Nothing

serializeSelectionValues
  :: MultiSelection
  -> Cell
  -> NonEmptyArray Column
  -> HashMap Cell CellValue
  -> String
serializeSelectionValues selection selectedCell columns tableData =
  intercalate newline
    $ intercalate tab
    <$> (foldMap show <<< (_ `HashMap.lookup` tableData))
    <$$> (getTargetCells selection selectedCell columns)

deserializeSelectionValues
  :: Cell -> NonEmptyArray Column -> String -> HashMap Cell CellValue
deserializeSelectionValues selectedCell columns str = HashMap.fromArray
  do
    rowValues /\ row <- zip' values (selectedCell.row .. maxRow)
    value /\ column <- zip' rowValues (selectedCell.column .. last columns)
    pure $ { row, column } /\ parseCellValue value
  where
  values = split (Pattern tab) <$> split (Pattern newline) str

getTargetCells
  :: MultiSelection
  -> Cell
  -> NonEmptyArray Column
  -> (NonEmptyArray (NonEmptyArray Cell))
getTargetCells selection selectedCell columns =
  fromMaybe (singleton $ singleton selectedCell) $ getSelectionCells selection
    columns

getSelectionCells
  :: MultiSelection
  -> NonEmptyArray Column
  -> Maybe (NonEmptyArray (NonEmptyArray Cell))
getSelectionCells selection columns = do
  columnBounds /\ rowBounds <- getSelectionBounds selection columns maxRowBounds
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
  -> NonEmptyArray Column
  -> NonEmptyArray Row
  -> Maybe (Tuple (NonEmptyArray Column) (NonEmptyArray Row))
getSelectionBounds NoSelection _ _ = Nothing
getSelectionBounds AllSelection columns rows =
  Just $ columns /\ rows
getSelectionBounds (ColumnsSelection origin target) _ rows =
  Just $ sort (origin .. target) /\ rows
getSelectionBounds (RowsSelection origin target) columns _ =
  Just $ columns /\ sort (origin .. target)
getSelectionBounds
  ( CellsSelection { column: column, row: row }
      { column: column', row: row' }
  )
  _
  _ =
  Just $ sort (column .. column') /\ sort (row .. row')

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
