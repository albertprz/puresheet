module App.Components.Table.Cell where

import FatPrelude
import Prim hiding (Row)

import Data.Int as Int

showCell :: Cell -> String
showCell { column, row } = show column <> show row

parseColumn :: String -> Maybe Column
parseColumn elemId = case toCharArray elemId of
  [ ch ] | isUpper ch -> Just $ Column ch
  _ -> Nothing

parseRow :: String -> Maybe Row
parseRow elemId = Row <$> fromString elemId

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

newtype Column = Column Char

derive newtype instance eqColumn :: Eq Column
derive newtype instance ordColumn :: Ord Column

newtype Row = Row Int

derive newtype instance eqRow :: Eq Row
derive newtype instance ordRow :: Ord Row

type Cell = { column :: Column, row :: Row }

data CellValue
  = StringVal String
  | IntVal Int

instance Show Column where
  show (Column x) = fromCharArray [ x ]

instance Show Row where
  show (Row x) = show x

instance Show CellValue where
  show (StringVal str) = str
  show (IntVal int) = show int
