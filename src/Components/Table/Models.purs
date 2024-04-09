module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Header, Row)
import App.Components.Table.Formula (Formula, FormulaId, FormulaState)
import App.Components.Table.Selection (MultiSelection, SelectionState)
import App.SyntaxTree.Common (Module, QVar, QVarOp)
import App.SyntaxTree.FnDef (FnInfo, OpInfo)
import App.Utils.KeyCode (KeyCode)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

type TableState =
  { selectedCell :: Cell
  , formulaCell :: Cell
  , activeFormula :: Boolean
  , activeInput :: Boolean
  , formulaState :: FormulaState
  , tableData :: HashMap Cell CellValue
  , tableFormulas :: HashMap Cell FormulaId
  , tableDependencies :: HashMap Cell (NonEmptySet FormulaId)
  , formulaCache :: HashMap FormulaId Formula
  , fnsMap :: HashMap QVar FnInfo
  , operatorsMap :: HashMap QVarOp OpInfo
  , aliasedModulesMap :: HashMap (Module /\ Module) (Set Module)
  , importedModulesMap :: HashMap Module (Set Module)
  , rows :: MinLenVect 1 Row
  , multiSelection :: MultiSelection
  , selectionState :: SelectionState
  , draggedHeader :: Maybe Header
  }

data TableAction
  = Initialize
  | WriteSelectedCellInput (Maybe Cell)
  | WriteFormulaCellInput (Maybe Cell)
  | WriteCell Cell CellValue
  | ClickHeader Header MouseEvent
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | FocusInCell Cell FocusEvent
  | KeyDown KeyCode KeyboardEvent
  | KeyUp KeyCode KeyboardEvent
  | SelectedCellInputKeyDown KeyCode KeyboardEvent
  | FormulaCellInputKeyDown KeyCode KeyboardEvent
  | FocusInEditor
  | FocusOutEditor
  | SubmitEditor String
  | WheelScroll WheelEvent
  | HoverCell EventTransition Cell MouseEvent
  | HoverHeader EventTransition Header MouseEvent
  | DragHeader EventTransition Header DragEvent
  | ResizeWindow

data EventTransition
  = Start
  | Over
  | End
