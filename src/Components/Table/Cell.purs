module App.Components.Table.Cell where

import FatPrelude
import Prim hiding (Row)
import Data.Int as Int

newtype Column = Column Char

newtype Row = Row Int

type Cell = { column :: Column, row :: Row }

data CellValue
  = StringVal String
  | IntVal Int

nextColumn :: Column -> Column
nextColumn (Column ch) = Column $ nextChar ch

prevColumn :: Column -> Column
prevColumn (Column ch) = Column $ prevChar ch

nextCell :: Cell -> Cell
nextCell cell = cell { column = nextColumn cell.column }

prevCell :: Cell -> Cell
prevCell cell = cell { column = prevColumn cell.column }

showCell :: Cell -> String
showCell { column, row } = show column <> show row

parseCellValue :: String -> CellValue
parseCellValue str =
  fromMaybe (StringVal str) (IntVal <$> Int.fromString str)

derive newtype instance eqColumn :: Eq Column
derive newtype instance ordColumn :: Ord Column

derive newtype instance eqRow :: Eq Row
derive newtype instance ordRow :: Ord Row

instance Show Column where
  show (Column x) = fromCharArray [ x ]

instance Show Row where
  show (Row x) = show x

instance Show CellValue where
  show (StringVal str) = str
  show (IntVal int) = show int
