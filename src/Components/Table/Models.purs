module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, Row)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type State =
  { selectedCell :: Cell
  , activeInput :: Boolean
  , tableData :: Map Cell CellValue
  , columns :: NonEmptyArray Column
  , rows :: NonEmptyArray Row
  , draggedHeader :: Maybe Column
  }

data Action
  = Initialize
  | WriteCell Cell CellValue
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | KeyPress String KeyboardEvent
  | InputKeyPress String KeyboardEvent
  | DragHeader Column 
  | DropHeader Column
  | DragOverHeader DragEvent

type CellMove = NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell
