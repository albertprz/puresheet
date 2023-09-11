module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, Header, MultiSelection, Row, SelectionState)
import App.SyntaxTrees.Common (Var, VarOp)
import App.SyntaxTrees.FnDef (FnInfo, OpInfo)
import App.Utils.Dom (KeyCode)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

type AppState =
  { selectedCell :: Cell
  , activeInput :: Boolean
  , formulaState :: FormulaState
  , tableData :: Map Cell CellValue
  , tableFormulas :: Map Cell String
  , columns :: NonEmptyArray Column
  , rows :: NonEmptyArray Row
  , multiSelection :: MultiSelection
  , selectionState :: SelectionState
  , draggedHeader :: Maybe Header
  , formulaCtx :: FormulaCtx
  }

type FormulaCtx =
  { fnsMap :: Map Var FnInfo
  , operatorsMap :: Map VarOp OpInfo
  }

data Action
  = Initialize
  | WriteCell Cell CellValue
  | ClickHeader Header MouseEvent
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | KeyPress KeyCode KeyboardEvent
  | KeyRelease KeyCode KeyboardEvent
  | FormulaKeyPress KeyCode KeyboardEvent
  | FormulaFocusOut FocusEvent
  | WheelScroll WheelEvent
  | HoverCell EventTransition Cell MouseEvent
  | HoverHeader EventTransition Header MouseEvent
  | DragHeader EventTransition Header DragEvent

data EventTransition
  = Start
  | Over
  | End

data FormulaState
  = ValidFormula
  | InValidFormula
  | UnknownFormula
