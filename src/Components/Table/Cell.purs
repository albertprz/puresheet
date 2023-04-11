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

nextCell :: NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
nextCell = getCell inc

prevCell :: NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
prevCell = getCell dec

nextColumnCell :: NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
nextColumnCell = getColumnCell inc

prevColumnCell :: NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
prevColumnCell = getColumnCell dec

nextRowCell :: NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
nextRowCell = getRowCell inc

prevRowCell :: NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
prevRowCell = getRowCell dec

showCell :: Cell -> String
showCell { column, row } = show column <> show row

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
getRowCell f _ rows { column, row } =
  (\row' -> { column, row: row' }) <$> getElemSat f rows row

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
