module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, Row)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

data Action
  = Initialize
  | WriteCell Cell CellValue
  | ClickCell Cell MouseEvent
  | KeyPress String KeyboardEvent
  | SelectCell (NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell)

type State =
  { selectedCell :: Cell
  , activeInput :: Boolean
  , tableData :: Map Cell CellValue
  , columns :: NonEmptyArray Column
  , rows :: NonEmptyArray Row
  }
