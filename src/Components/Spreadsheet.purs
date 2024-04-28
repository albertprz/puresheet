module App.Components.Spreadsheet where

import FatPrelude
import Prim hiding (Row)

import App.AppM (AppM)
import App.Components.Spreadsheet.Cell (mkColumn, mkRow)
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Components.Spreadsheet.Handler (handleAction)
import App.Components.Spreadsheet.Models (SpreadsheetAction(..), SpreadsheetInput, SpreadsheetState, TableDataState)
import App.Components.Spreadsheet.Renderer (render)
import App.Components.Spreadsheet.Selection (MultiSelection(..), SelectionState(..))
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Data.Slot (Slot)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectEq)
import Record (merge)
import Record.Extra (pick)

component :: forall q. Component q SpreadsheetInput Unit AppM
component =
  connect (selectEq pick)
    $ mkComponent
        { initialState
        , render
        , eval: mkEval defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }

initialState
  :: { context :: TableDataState, input :: SpreadsheetInput }
  -> SpreadsheetState
initialState { context, input } =
  merge context $ merge input
    { selectedCell: { column: mkColumn 'A', row: mkRow 1 }
    , formulaCell: { column: mkColumn 'A', row: mkRow 1 }
    , activeFormula: false
    , activeInput: false
    , formulaState: UnknownFormula
    , rows: mkRow <$> (0 .. 100)
    , multiSelection: NoSelection
    , selectionState: NotStartedSelection
    , draggedHeader: Nothing
    }

type SpreadsheetSlot = forall q. Slot q Unit Unit

_spreadsheet :: Proxy "spreadsheet"
_spreadsheet = Proxy
