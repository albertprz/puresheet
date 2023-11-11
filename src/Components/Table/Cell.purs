module App.Components.Table.Cell where

import FatPrelude
import Prim hiding (Row)

import App.Utils.Bounded (newtypeCardinality, newtypeFromEnum, newtypeToEnum)
import App.Utils.HashMap (swapKey) as HashMap
import Bookhound.Parser (Parser, runParser)
import Bookhound.ParserCombinators (is)
import Bookhound.Parsers.Char (anyChar, upper)
import Bookhound.Parsers.Number (double, int, unsignedInt)
import Data.HashMap (keys) as HashMap
import Data.String.CodeUnits as String

parseCellValue :: String -> CellValue
parseCellValue input =
  fromRight (StringVal input) (runParser cellValueParser input)

rowParser :: Parser Row
rowParser = Row <$> unsignedInt

columnParser :: Parser Column
columnParser = Column <<< fromUpper <$> upper

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
  -> Cell
  -> Maybe Cell
getCell f cell =
  getColumnCell (over Column f) cell
    <|> getRowCell (over Row f) cell

getColumnCell
  :: (Column -> Column)
  -> Cell
  -> Maybe Cell
getColumnCell f { column, row } =
  ({ column: _, row }) <$> getInBoundedRange (f column)

getRowCell
  :: (Row -> Row)
  -> Cell
  -> Maybe Cell
getRowCell f { column, row } =
  ({ column, row: _ }) <$> getInBoundedRange (f row)

prevColumnCell :: Cell -> Cell
prevColumnCell { column, row } = { column: dec column, row }

nextColumnCell :: Cell -> Cell
nextColumnCell { column, row } = { column: inc column, row }

prevRowCell :: Cell -> Cell
prevRowCell { column, row } = { column, row: dec row }

nextRowCell :: Cell -> Cell
nextRowCell { column, row } = { column, row: inc row }

getColumnHeader :: Header -> Maybe Column
getColumnHeader (ColumnHeader header) = Just header
getColumnHeader _ = Nothing

getRowHeader :: Header -> Maybe Row
getRowHeader (RowHeader header) = Just header
getRowHeader _ = Nothing

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

allColumns :: NonEmptyArray Column
allColumns = allValues

allRows :: NonEmptyArray Row
allRows = allValues

newtype Column = Column Int

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
derive newtype instance Semiring Column
derive newtype instance Ring Column
derive newtype instance Enum Column
derive instance Newtype Column _

instance Show Column where
  show = String.singleton <<< toUpper <<< unwrap

instance Hashable Column where
  hash = unwrap

instance Bounded Column where
  bottom = zero
  top = wrap (upperEndCode - upperStartCode)

instance BoundedEnum Column where
  cardinality = newtypeCardinality
  fromEnum = newtypeFromEnum
  toEnum = newtypeToEnum

derive newtype instance Eq Row
derive newtype instance Ord Row
derive newtype instance Semiring Row
derive newtype instance Ring Row
derive newtype instance Enum Row
derive instance Newtype Row _

instance Show Row where
  show (Row x) = show x

instance Hashable Row where
  hash = unwrap

instance Bounded Row where
  bottom = one
  top = wrap 10_000

instance BoundedEnum Row where
  cardinality = newtypeCardinality
  fromEnum = newtypeFromEnum
  toEnum = newtypeToEnum

derive instance Eq CellValue

instance Show CellValue where
  show (BoolVal x) = show x
  show (IntVal x) = show x
  show (FloatVal x) = show x
  show (CharVal x) = String.singleton x
  show (StringVal x) = x
