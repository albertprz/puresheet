module App.Components.Spreadsheet.Models where

import FatPrelude
import Prim hiding (Row)

import App.AppStore (Store)
import App.Components.Editor (EditorSlot)
import App.Components.Spreadsheet.Cell (Cell, CellValue, Header, Row)
import App.Components.Spreadsheet.Formula (Formula, FormulaId, FormulaState)
import App.Components.Spreadsheet.Selection (MultiSelection, SelectionState)
import App.Editor.Suggestion (SuggestionTerm)
import App.Routes (Route)
import App.Utils.KeyCode (KeyCode)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

type SpreadsheetState =
  { route :: Route
  , store :: Store
  , selectedCell :: Cell
  , formulaCell :: Cell
  , activeFormula :: Boolean
  , activeInput :: Boolean
  , formulaState :: FormulaState
  , rows :: NonEmptyArray Row
  , multiSelection :: MultiSelection
  , selectionState :: SelectionState
  , draggedHeader :: Maybe Header
  | TableDataStateRow
  }

type TableDataStateRow =
  ( tableData :: HashMap Cell CellValue
  , tableFormulas :: HashMap Cell FormulaId
  , tableDependencies :: HashMap Cell (NonEmptySet FormulaId)
  , formulaCache :: HashMap FormulaId Formula
  )

type TableDataState = Record TableDataStateRow

data SpreadsheetAction
  = WriteSelectedCellInput (Maybe Cell)
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
  | GoToDefinition (Maybe SuggestionTerm)
  | ResizeWindow
  | Initialize
  | Receive { context :: Store, input :: SpreadsheetInput }

type SpreadsheetInput = { route :: Route }

type Slots = (editor :: EditorSlot)

data EventTransition
  = Start
  | Over
  | End
