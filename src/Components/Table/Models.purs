module App.Components.Table.Models where

import FatPrelude
import Prim hiding (Row)

import App.CSS.ClassNames (invalidFormula, unknownFormula, validFormula)
import App.Components.Table.Cell (Cell, CellValue, Column, Header, MultiSelection, Row, SelectionState)
import App.SyntaxTrees.Common (Var, VarOp)
import App.SyntaxTrees.FnDef (FnInfo, OpInfo, Scope)
import App.Utils.Dom (KeyCode)
import Web.HTML (ClassName)
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
  -- , tableFormulas :: Map Cell FormulaId
  -- , tableDependencies :: Map Cell (NonEmptySet FormulaId)
  -- , formulasCache :: Map FormulaId Formula
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
  | WriteCell Cell CellValue
  | ClickHeader Header MouseEvent
  | ClickCell Cell MouseEvent
  | DoubleClickCell Cell MouseEvent
  | FocusInCell Cell FocusEvent
  | KeyPress KeyCode KeyboardEvent
  | KeyRelease KeyCode KeyboardEvent
  | FormulaKeyPress KeyCode KeyboardEvent
  | WheelScroll WheelEvent
  | HoverCell EventTransition Cell MouseEvent
  | HoverHeader EventTransition Header MouseEvent
  | DragHeader EventTransition Header DragEvent

newtype FormulaId = FormulaId Int

newtype Formula = Formula String

data EventTransition
  = Start
  | Over
  | End

data FormulaState
  = ValidFormula
  | InvalidFormula
  | UnknownFormula

derive instance Eq FormulaState

formulaStateToClass :: FormulaState -> ClassName
formulaStateToClass = case _ of
  ValidFormula -> validFormula
  InvalidFormula -> invalidFormula
  UnknownFormula -> unknownFormula
