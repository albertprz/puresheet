module App.CSS.ClassNames where

import Halogen (ClassName(..))

headerMenu :: ClassName
headerMenu = ClassName "header-menu"

navButton :: ClassName
navButton = ClassName "nav-button"

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

invisibleContainer :: ClassName
invisibleContainer = ClassName "invisible-container"

spreadsheetContainer :: ClassName
spreadsheetContainer = ClassName "spreadsheet-container"

explorerContainer :: ClassName
explorerContainer = ClassName "explorer-container"

formulaSectionContainer :: ClassName
formulaSectionContainer = ClassName "formula-container"

formulaBoxContainer :: ClassName
formulaBoxContainer = ClassName "formula-box-container"

selectedCellInput :: ClassName
selectedCellInput = ClassName "selected-cell-input"

formulaCellInput :: ClassName
formulaCellInput = ClassName "formula-cell-input"

functionSignature :: ClassName
functionSignature = ClassName "function-signature"

suggestionsDropdown :: ClassName
suggestionsDropdown = ClassName "suggestions-dropdown"

suggestionOption :: ClassName
suggestionOption = ClassName "suggestion-option"

selectedSuggestionOption :: ClassName
selectedSuggestionOption = ClassName "selected-suggestion-option"

cellSyntax :: ClassName
cellSyntax = ClassName "cell-syntax"

numberSyntax :: ClassName
numberSyntax = ClassName "number-syntax"

stringSyntax :: ClassName
stringSyntax = ClassName "string-syntax"

keywordSyntax :: ClassName
keywordSyntax = ClassName "keyword-syntax"

symbolSyntax :: ClassName
symbolSyntax = ClassName "symbol-syntax"

operatorSyntax :: ClassName
operatorSyntax = ClassName "operator-syntax"

functionSyntax :: ClassName
functionSyntax = ClassName "function-syntax"

regularSyntax :: ClassName
regularSyntax = ClassName "regular-syntax"

materialIcons :: ClassName
materialIcons = ClassName "material-icons"
