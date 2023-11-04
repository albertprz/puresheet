module App.CSS.Ids where

import Prelude

cellId :: ElementId
cellId = ElementId "cell"

formulaBoxId :: ElementId
formulaBoxId = ElementId "formula-box"

selectedCellInputId :: ElementId
selectedCellInputId = ElementId "selected-cell-input"

formulaCellInputId :: ElementId
formulaCellInputId = ElementId "formula-cell-input"

formulaSignatureId :: ElementId
formulaSignatureId = ElementId "formula-signature"

inputElement :: ElementType
inputElement = ElementType "input"

newtype ElementId = ElementId String

newtype ElementType = ElementType String

instance Show ElementId where
  show (ElementId x) = x

instance Show ElementType where
  show (ElementType x) = x
