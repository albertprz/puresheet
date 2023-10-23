module App.CSS.Ids where

import Prelude

newtype ElementId = ElementId String

newtype ElementType = ElementType String

instance Show ElementId where
  show (ElementId x) = x

instance Show ElementType where
  show (ElementType x) = x

formulaBoxId :: ElementId
formulaBoxId = ElementId "formula-box"

cellId :: ElementId
cellId = ElementId "cell"

selectedCellInputId :: ElementId
selectedCellInputId = ElementId "selected-cell-input"

formulaCellInputId :: ElementId
formulaCellInputId = ElementId "formula-cell-input"

inputElement :: ElementType
inputElement = ElementType "input"
