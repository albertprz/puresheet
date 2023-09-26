module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, Header, MultiSelection, Row, SelectionState)
import App.Components.Table.Formula (Formula, FormulaId, FormulaState)
import App.SyntaxTree.Common (Var, VarOp)
import App.SyntaxTree.FnDef (FnInfo, OpInfo, Scope)
import App.Utils.Dom (KeyCode)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

type AppState =
  { selectedCell :: Cell
  , formulaCell :: Cell
  , activeFormula :: Boolean
  , activeInput :: Boolean
  , formulaState :: FormulaState
  , tableData :: Map Cell CellValue
  , tableFormulas :: Map Cell FormulaId
  , tableDependencies :: Map Cell (NonEmptySet FormulaId)
  , formulaCache :: Map FormulaId Formula
  , columns :: NonEmptyArray Column
  , rows :: NonEmptyArray Row
  , multiSelection :: MultiSelection
  , selectionState :: SelectionState
  , draggedHeader :: Maybe Header
  , formulaCtx :: FormulaCtx
  }

type FormulaCtx =
  { fnsMap :: Map (Scope /\ Var) FnInfo
  , operatorsMap :: Map VarOp OpInfo
  }

data Action
  = Initialize
  | WriteSelectedCellInput (Maybe Cell)
  | WriteFormulaCellInput (Maybe Cell)
  | WriteCell Cell CellValue
  | ClickHeader Header MouseEvent
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | FocusInCell Cell FocusEvent
  | KeyPress KeyCode KeyboardEvent
  | KeyRelease KeyCode KeyboardEvent
  | FormulaKeyPress KeyCode KeyboardEvent
  | SelectedCellInputKeyPress KeyCode KeyboardEvent
  | FormulaCellInputKeyPress KeyCode KeyboardEvent
  | FocusInFormula FocusEvent
  | WheelScroll WheelEvent
  | HoverCell EventTransition Cell MouseEvent
  | HoverHeader EventTransition Header MouseEvent
  | DragHeader EventTransition Header DragEvent

data EventTransition
  = Start
  | Over
  | End

