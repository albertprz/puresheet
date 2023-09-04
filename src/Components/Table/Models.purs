module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, Header, MultiSelection, Row, SelectionState)
import App.SyntaxTrees.Common (Literal, Var, VarOp)
import App.Utils.Dom (KeyCode)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

type AppState =
  { selectedCell :: Cell
  , activeInput :: Boolean
  , tableData :: Map Cell CellValue
  , columns :: NonEmptyArray Column
  , rows :: NonEmptyArray Row
  , multiSelection :: MultiSelection
  , selectionState :: SelectionState
  , draggedHeader :: Maybe Header
  , fnsMap :: Map Var ((Array Literal -> Literal) /\ Int)
  , operatorsMap :: Map VarOp Var
  , precedenceMap :: Map VarOp Int
  , rAssocSet :: Set VarOp
  }

data Action
  = Initialize
  | WriteCell Cell CellValue
  | ClickHeader Header MouseEvent
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | KeyPress KeyCode KeyboardEvent
  | KeyRelease KeyCode KeyboardEvent
  | InputKeyPress KeyCode KeyboardEvent
  | WheelScroll WheelEvent
  | HoverCell EventTransition Cell MouseEvent
  | HoverHeader EventTransition Header MouseEvent
  | DragHeader EventTransition Header DragEvent

data EventTransition
  = Start
  | Over
  | End

