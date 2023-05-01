module App.Components.Table.Cell where

import FatPrelude
import Prim hiding (Row)

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple)

showCell :: Cell -> String
showCell { column, row } = show column <> show row

parseColumn :: String -> Maybe Column
parseColumn elemId = case toCharArray elemId of
  [ ch ] | isUpper ch -> Just $ Column ch
  _ -> Nothing

parseRow :: String -> Maybe Row
parseRow elemId = Row <$> fromString elemId

showCellValue :: CellValue -> String
showCellValue (StringVal x) = x
showCellValue (IntVal x) = show x

parseCellValue :: String -> CellValue
parseCellValue str =
  fromMaybe (StringVal str) (IntVal <$> Int.fromString str)

getCell :: (Int -> Int) -> NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
getCell f columns rows = getElemSat f cells
  where
  cells = do
    row <- rows
    column <- columns
    pure { column, row }

getColumnCell :: (Int -> Int) -> NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
getColumnCell f columns _ { column, row } =
  (\column' -> { column: column', row }) <$> getElemSat f columns column

getRowCell :: (Int -> Int) -> NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
getRowCell f _ _ { column, row: Row (rowNum) } =
  Just { column, row: Row $ max one $ f rowNum }

nextColumn :: Cell -> Cell
nextColumn { column: Column col, row } =
  { column: Column $ nextChar col, row }

prevColumn :: Cell -> Cell
prevColumn { column: Column col, row } =
  { column: Column $ prevChar col, row }

nextRow :: Cell -> Cell
nextRow { column, row: Row row } =
  { column, row: Row $ inc row }

prevRow :: Cell -> Cell
prevRow { column, row: Row row } =
  { column, row: Row $ dec row }

isColumnSelected :: Cell -> MultiSelection -> Column -> Boolean
isColumnSelected selectedCell multiSelection column =
  selectedCell.column == column ||
    isColumnInSelection multiSelection column

isRowSelected :: Cell -> MultiSelection -> Row -> Boolean
isRowSelected selectedCell multiSelection row =
  selectedCell.row == row ||
    isRowInSelection multiSelection row

isCellAtRightSelection :: MultiSelection -> Cell -> Boolean
isCellAtRightSelection selection cell = (not $ isCellInSelection selection cell) && (isCellInSelection selection $ prevColumn cell)

isCellAboveSelection :: MultiSelection -> Cell -> Boolean
isCellAboveSelection selection cell = (not $ isCellInSelection selection cell) && (isCellInSelection selection $ nextRow cell)

isCellAtLeftSelection :: MultiSelection -> Cell -> Boolean
isCellAtLeftSelection selection cell = (isCellInSelection selection cell) && (not $ isCellInSelection selection $ prevColumn cell)

isCellBelowSelection :: MultiSelection -> Cell -> Boolean
isCellBelowSelection selection cell = (isCellInSelection selection cell) && (not $ isCellInSelection selection $ nextRow cell)

isColumnInSelection :: MultiSelection -> Column -> Boolean
isColumnInSelection NoSelection _ = false
isColumnInSelection AllSelection _ = true
isColumnInSelection (RowSelection _) _ = true
isColumnInSelection (ColumnSelection columns) col = elem col columns
isColumnInSelection (CellsSelection origin target) col =
  inRange origin.column target.column col

isRowInSelection :: MultiSelection -> Row -> Boolean
isRowInSelection NoSelection _ = false
isRowInSelection AllSelection _ = true
isRowInSelection (ColumnSelection _) _ = true
isRowInSelection (RowSelection rows) row = elem row rows
isRowInSelection (CellsSelection origin target) row =
  inRange origin.row target.row row

isCellInSelection :: MultiSelection -> Cell -> Boolean
isCellInSelection NoSelection _ = false
isCellInSelection AllSelection _ = true
isCellInSelection (ColumnSelection cols) cell = elem cell.column cols
isCellInSelection (RowSelection rows) cell = elem cell.row rows
isCellInSelection (CellsSelection origin target) { column, row } =
  inRange origin.row target.row row &&
    inRange origin.column target.column column

getSelectionTargetCell :: MultiSelection -> Maybe Cell
getSelectionTargetCell (CellsSelection _ target) = Just target
getSelectionTargetCell _ = Nothing

getCellFromMove :: CellMove -> NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Cell
getCellFromMove move columns rows cell =
  fromMaybe cell $ (interpretCellMove move) columns rows cell

interpretCellMove :: CellMove -> (NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell)
interpretCellMove = case _ of
  NextRow -> getRowCell inc
  PrevRow -> getRowCell dec
  NextColumn -> getColumnCell inc
  PrevColumn -> getColumnCell dec
  NextCell -> getCell inc
  PrevCell -> getCell dec
  OtherColumn column -> \_ _ cell -> Just $ cell { column = column }
  OtherRow row -> \_ _ cell -> Just $ cell { row = row }
  OtherCell cell -> \_ _ _ -> Just cell

serializeSelectionValues :: MultiSelection -> Cell -> NonEmptyArray Column -> Map Cell CellValue -> String
serializeSelectionValues selection selectedCell columns tableData =
  intercalate newline
    $ intercalate tab
    <$> (foldMap showCellValue <<< (_ `Map.lookup` tableData))
    <$$> (getTargetCells selection selectedCell columns)

deserializeSelectionValues :: Cell -> NonEmptyArray Column -> String -> Map Cell CellValue
deserializeSelectionValues selectedCell columns str = Map.fromFoldable
  do
    rowValues /\ row <- Array.zip values $ toArray (selectedCell.row .. maxRow)
    value /\ column <- Array.zip rowValues $ toArray (selectedCell.column .. last columns)
    pure $ { row, column } /\ parseCellValue value
  where
  values = split (Pattern tab) <$> split (Pattern newline) str

getTargetCells :: MultiSelection -> Cell -> NonEmptyArray Column -> (NonEmptyArray (NonEmptyArray Cell))
getTargetCells selection selectedCell columns =
  fromMaybe (singleton $ singleton selectedCell) $ getSelectionCells selection columns

getSelectionCells :: MultiSelection -> NonEmptyArray Column -> Maybe (NonEmptyArray (NonEmptyArray Cell))
getSelectionCells selection columns = do
  columnBounds /\ rowBounds <- getSelectionBounds selection columns maxRowBounds
  pure do
    row <- rowBounds
    pure do
      column <- columnBounds
      pure $ { column, row }

getSelectionBounds :: MultiSelection -> NonEmptyArray Column -> NonEmptyArray Row -> Maybe (Tuple (NonEmptyArray Column) (NonEmptyArray Row))
getSelectionBounds NoSelection _ _ = Nothing
getSelectionBounds AllSelection columns rows =
  Just $ columns /\ rows
getSelectionBounds (ColumnSelection columns) _ rows =
  Just $ sort columns /\ rows
getSelectionBounds (RowSelection rows) columns _ =
  Just $ columns /\ sort rows
getSelectionBounds
  ( CellsSelection { column: column, row: row }
      { column: column', row: row' }
  )
  _
  _ =
  Just $ sort (column .. column') /\ sort (row .. row')

getColumnHeader :: Header -> Maybe Column
getColumnHeader (ColumnHeader header) = Just header
getColumnHeader _ = Nothing

getRowHeader :: Header -> Maybe Row
getRowHeader (RowHeader header) = Just header
getRowHeader _ = Nothing

firstRow :: Row
firstRow = Row 1

maxRow :: Row
maxRow = Row 1_000

maxRowBounds :: NonEmptyArray Row
maxRowBounds = firstRow .. maxRow

swapTableMapColumn :: forall v. Column -> Column -> Map Cell v -> Map Cell v
swapTableMapColumn origin target tableDict =
  foldl (flip swapMapKey) tableDict keysToSwap
  where
  keysToSwap = Set.map (\row -> { column: origin, row } /\ { column: target, row })
    $ Set.map (\cell -> cell.row)
    $ Set.filter
        (\cell -> cell.column == origin || cell.column == target)
        (Map.keys tableDict)

swapTableMapRow :: forall v. Row -> Row -> Map Cell v -> Map Cell v
swapTableMapRow origin target tableDict =
  foldl (flip swapMapKey) tableDict keysToSwap
  where
  keysToSwap = Set.map (\column -> { column, row: origin } /\ { column, row: target })
    $ Set.map (\cell -> cell.column)
    $ Set.filter
        (\cell -> cell.row == origin || cell.row == target)
        (Map.keys tableDict)

swapMapKey :: forall k v. Ord k => Tuple k k -> Map k v -> Map k v
swapMapKey (k1 /\ k2) dict =
  Map.alter (const v2) k1 $
    Map.alter (const v1) k2 dict
  where
  v1 = Map.lookup k1 dict
  v2 = Map.lookup k2 dict

newtype Column = Column Char

newtype Row = Row Int

type Cell = { column :: Column, row :: Row }

data CellValue
  = StringVal String
  | IntVal Int

data CellMove
  = NextRow
  | PrevRow
  | NextColumn
  | PrevColumn
  | NextCell
  | PrevCell
  | OtherCell Cell
  | OtherColumn Column
  | OtherRow Row

data MultiSelection
  = RowSelection (NonEmptyArray Row)
  | ColumnSelection (NonEmptyArray Column)
  | CellsSelection Cell Cell
  | AllSelection
  | NoSelection

data SelectionState
  = InProgressSelection
  | NotStartedSelection
  | CopySelection

data Header
  = CornerHeader
  | ColumnHeader Column
  | RowHeader Row

derive newtype instance Eq Column
derive newtype instance Ord Column
derive newtype instance Eq Row
derive newtype instance Ord Row
derive instance Eq SelectionState

instance Show Column where
  show (Column x) = fromCharArray [ x ]

instance Show Row where
  show (Row x) = show x

instance Show CellValue where
  show (StringVal str) = str
  show (IntVal int) = show int

instance Range Column where
  range (Column c1) (Column c2) = Column <$> c1 .. c2

instance Range Row where
  range (Row r1) (Row r2) = Row <$> r1 .. r2
