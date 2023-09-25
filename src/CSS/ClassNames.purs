module App.CSS.ClassNames where

import Halogen (ClassName(..))

sheet :: ClassName
sheet = ClassName "sheet"

strippedSheet :: ClassName
strippedSheet = ClassName "stripped-sheet"

selectedSheetCell :: ClassName
selectedSheetCell = ClassName "selected-sheet-cell"

inSelection :: ClassName
inSelection = ClassName "in-selection"

copySelection :: ClassName
copySelection = ClassName "copy-selection"

aboveSelection :: ClassName
aboveSelection = ClassName "above-selection"

belowSelection :: ClassName
belowSelection = ClassName "below-selection"

atLeftSelection :: ClassName
atLeftSelection = ClassName "at-left-selection"

atRightSelection :: ClassName
atRightSelection = ClassName "at-right-selection"

sheetCell :: ClassName
sheetCell = ClassName "sheet-cell"

formulaBox :: ClassName
formulaBox = ClassName "formula-box"

validFormula :: ClassName
validFormula = ClassName "valid-formula"

invalidFormula :: ClassName
invalidFormula = ClassName "invalid-formula"

unknownFormula :: ClassName
unknownFormula = ClassName "unknown-formula"

columnHeader :: ClassName
columnHeader = ClassName "column-header"

selectedHeader :: ClassName
selectedHeader = ClassName "selected-header"

rowHeader :: ClassName
rowHeader = ClassName "row-header"

cornerHeader :: ClassName
cornerHeader = ClassName "corner-header"

mainContainer :: ClassName
mainContainer = ClassName "main-container"

formulaContainer :: ClassName
formulaContainer = ClassName "formula-container"

selectedCellInput :: ClassName
selectedCellInput = ClassName "selected-cell-input"

formulaCellInput :: ClassName
formulaCellInput = ClassName "formula-cell-input"
