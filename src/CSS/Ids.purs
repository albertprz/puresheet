module App.CSS.Ids where

import Prelude

import App.Components.Spreadsheet.Cell (Cell, showCell)
import Data.Newtype (class Newtype)

cellId :: Cell -> ElementId
cellId cell = ElementId $ "cell" <> showCell cell

functionRowId :: Int -> ElementId
functionRowId n = ElementId $ "functionRow" <> show n

spreadsheetTableId :: ElementId
spreadsheetTableId = ElementId "spreadsheet-table"

formulaBoxId :: ElementId
formulaBoxId = ElementId "formula-box"

selectedCellInputId :: ElementId
selectedCellInputId = ElementId "selected-cell-input"

formulaCellInputId :: ElementId
formulaCellInputId = ElementId "formula-cell-input"

functionSignatureId :: ElementId
functionSignatureId = ElementId "function-signature"

suggestionsDropdownId :: ElementId
suggestionsDropdownId = ElementId "suggestions-dropdown"

inputElementType :: ElementType
inputElementType = ElementType "input"

newtype ElementId = ElementId String

newtype ElementType = ElementType String

derive instance Newtype ElementId _

derive instance Newtype ElementType _

instance Show ElementId where
  show (ElementId x) = x

instance Show ElementType where
  show (ElementType x) = x
