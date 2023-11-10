module App.Components.Table.Cell where

import FatPrelude
import Prim hiding (Row)

import App.Utils.HashMap (swapKey) as HashMap
import Bookhound.Parser (Parser, runParser)
import Bookhound.ParserCombinators (is)
import Bookhound.Parsers.Char (anyChar, upper)
import Bookhound.Parsers.Number (double, int, unsignedInt)
import Data.HashMap (keys) as HashMap
import Data.Set as Set
import Data.String.CodeUnits as String

parseCellValue :: String -> CellValue
parseCellValue input =
  fromRight (StringVal input) (runParser cellValueParser input)

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
    <$> anyChar

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

swapTableMapColumn
  :: forall v. Column -> Column -> HashMap Cell v -> HashMap Cell v
swapTableMapColumn origin target tableDict =
  foldl (flip HashMap.swapKey) tableDict keysToSwap
  where
  keysToSwap =
    map (\row -> { column: origin, row } /\ { column: target, row })
      $ map (\cell -> cell.row)
      $ filter
          (\cell -> cell.column == origin || cell.column == target)
          (HashMap.keys tableDict)

swapTableMapRow :: forall v. Row -> Row -> HashMap Cell v -> HashMap Cell v
swapTableMapRow origin target tableDict =
  foldl (flip HashMap.swapKey) tableDict keysToSwap
  where
  keysToSwap =
    map (\column -> { column, row: origin } /\ { column, row: target })
      $ map (\cell -> cell.column)
      $ filter
          (\cell -> cell.row == origin || cell.row == target)
          (HashMap.keys tableDict)

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
  show (Column x) = String.singleton x

instance Hashable Column where
  hash = hash <<< show

instance Show Row where
  show (Row x) = show x

instance Hashable Row where
  hash = hash <<< show

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
