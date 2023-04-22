module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, MultiSelection, Row, SelectionState)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

type State =
  { selectedCell :: Cell
  , activeInput :: Boolean
  , tableData :: Map Cell CellValue
  , columns :: NonEmptyArray Column
  , rows :: NonEmptyArray Row
  , multiSelection :: MultiSelection
  , selectionState :: SelectionState
  , draggedHeader :: Maybe Column
  }

data Action
  = Initialize
  | WriteCell Cell CellValue
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | KeyPress KeyCode KeyboardEvent
  | InputKeyPress KeyCode KeyboardEvent
  | WheelScroll WheelEvent
  | ClickHeader Header
  | DragHeader EventTransition Column DragEvent
  | DragCell EventTransition Cell MouseEvent

data EventTransition
  = Start
  | Over
  | End

data Header
  = CornerHeader
  | ColumnHeader Column
  | RowHeader Row

data KeyCode
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Enter
  | Tab
  | Space
  | OtherKey String
