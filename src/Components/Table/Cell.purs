module App.Components.Table.Cell where

import FatPrelude
import Prim hiding (Row)

import App.Utils.Map (swapKey) as Map
import Bookhound.Parser (Parser, char, runParser)
import Bookhound.ParserCombinators (is, (->>-), (<|>), (|?))
import Bookhound.Parsers.Char (dash, dot, upper)
import Bookhound.Parsers.Number (negInt, unsignedInt)
import Bookhound.Utils.UnsafeRead (unsafeRead)
import Data.Map (keys) as Map
import Data.Set as Set
import Data.String.CodeUnits as String

rowParser :: Parser Row
rowParser = Row <$> unsignedInt

columnParser :: Parser Column
columnParser = Column <$> upper

cellParser :: Parser Cell
cellParser = do
  column <- columnParser
  row <- rowParser
  pure { column, row }

cellValueParser :: Parser CellValue
cellValueParser =
  FloatVal
    <$> double
    <|> IntVal
    <$> int
    <|> BoolVal
    <$> (true <$ is "true" <|> false <$ is "false")
    <|> CharVal
    <$> char

int :: Parser Int
int = unsignedInt <|> negInt

double :: Parser Number
double = unsafeRead
  <$> (|?) dash
  ->>- unsignedInt
  ->>- dot
  ->>- unsignedInt

showCell :: Cell -> String
showCell { column, row } = show column <> show row

getCell
  :: (Int -> Int)
  -> NonEmptyArray Column
  -> NonEmptyArray Row
  -> Cell
  -> Maybe Cell
getCell f columns rows = getElemSat f cells
  where
  cells = do
    row <- rows
    column <- columns
    pure { column, row }

getColumnCell
  :: (Int -> Int)
  -> NonEmptyArray Column
  -> NonEmptyArray Row
  -> Cell
  -> Maybe Cell
getColumnCell f columns _ { column, row } =
  (\column' -> { column: column', row }) <$> getElemSat f columns column

getRowCell
  :: (Int -> Int)
  -> NonEmptyArray Column
  -> NonEmptyArray Row
  -> Cell
  -> Maybe Cell
getRowCell f _ _ { column, row: Row rowNum } =
  Just { column, row: Row $ max one $ f rowNum }

nextColumnCell :: Cell -> Cell
nextColumnCell { column, row } =
  { column: nextColumn column, row }

prevColumnCell :: Cell -> Cell
prevColumnCell { column, row } =
  { column: prevColumn column, row }

nextRowCell :: Cell -> Cell
nextRowCell { column, row } =
  { column, row: nextRow row }

prevRowCell :: Cell -> Cell
prevRowCell { column, row } =
  { column, row: prevRow row }

nextColumn :: Column -> Column
nextColumn (Column column) = Column $ nextChar column

prevColumn :: Column -> Column
prevColumn (Column column) = Column $ prevChar column

nextRow :: Row -> Row
nextRow (Row row) = Row $ inc row

prevRow :: Row -> Row
prevRow (Row row) = Row $ dec row

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
  foldl (flip Map.swapKey) tableDict keysToSwap
  where
  keysToSwap =
    Set.map (\row -> { column: origin, row } /\ { column: target, row })
      $ Set.map (\cell -> cell.row)
      $ Set.filter
          (\cell -> cell.column == origin || cell.column == target)
          (Map.keys tableDict)

swapTableMapRow :: forall v. Row -> Row -> Map Cell v -> Map Cell v
swapTableMapRow origin target tableDict =
  foldl (flip Map.swapKey) tableDict keysToSwap
  where
  keysToSwap =
    Set.map (\column -> { column, row: origin } /\ { column, row: target })
      $ Set.map (\cell -> cell.column)
      $ Set.filter
          (\cell -> cell.row == origin || cell.row == target)
          (Map.keys tableDict)

buildCell :: Column -> Row -> Cell
buildCell column row = { column, row }

newtype Column = Column Char

newtype Row = Row Int

type Cell = { column :: Column, row :: Row }

data CellValue
  = BoolVal Boolean
  | IntVal Int
  | FloatVal Number
  | CharVal Char
  | StringVal String

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

data Header
  = CornerHeader
  | ColumnHeader Column
  | RowHeader Row

derive newtype instance Eq Column
derive newtype instance Ord Column
derive newtype instance Eq Row
derive newtype instance Ord Row
derive instance Eq CellValue

instance Show Column where
  show (Column x) = fromCharArray [ x ]

instance Show Row where
  show (Row x) = show x

instance Show CellValue where
  show (BoolVal x) = show x
  show (IntVal x) = show x
  show (FloatVal x) = show x
  show (CharVal x) = String.singleton x
  show (StringVal x) = x

instance Range Column where
  range (Column c1) (Column c2) = Column <$> c1 .. c2

instance Range Row where
  range (Row r1) (Row r2) = Row <$> r1 .. r2
