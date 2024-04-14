module App.Components.Spreadsheet where

import FatPrelude
import Prim hiding (Row)

import App.AppM (AppM)
import App.Components.Spreadsheet.Cell (CellValue(..), mkColumn, mkRow)
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Components.Spreadsheet.Handler (handleAction)
import App.Components.Spreadsheet.Models (SpreadsheetAction(..), SpreadsheetInput, SpreadsheetState)
import App.Components.Spreadsheet.Renderer (render)
import App.Components.Spreadsheet.Selection (MultiSelection(..), SelectionState(..))
import Data.HashMap as HashMap
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Data.Slot (Slot)

component :: forall q. Component q SpreadsheetInput Unit AppM
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState :: SpreadsheetInput -> SpreadsheetState
initialState { route } =
  { route
  , selectedCell: { column: mkColumn 'A', row: mkRow 1 }
  , formulaCell: { column: mkColumn 'A', row: mkRow 1 }
  , activeFormula: false
  , activeInput: false
  , formulaState: UnknownFormula
  , tableData: HashMap.fromArray
      [ { column: mkColumn 'A', row: mkRow 1 } /\ IntVal 1
      , { column: mkColumn 'B', row: mkRow 1 } /\ IntVal 2
      , { column: mkColumn 'C', row: mkRow 1 } /\ IntVal 3
      , { column: mkColumn 'D', row: mkRow 1 } /\ IntVal 4
      ]
  , tableDependencies: HashMap.empty
  , tableFormulas: HashMap.empty
  , formulaCache: HashMap.empty
  , rows: mkRow <$> (0 .. 100)
  , multiSelection: NoSelection
  , selectionState: NotStartedSelection
  , draggedHeader: Nothing
  }

type SpreadsheetSlot = forall q. Slot q Unit Unit

_spreadsheet :: Proxy "spreadsheet"
_spreadsheet = Proxy
