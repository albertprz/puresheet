module App.Components.Table where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (CellValue(..), mkColumn, mkRow)
import App.Components.Table.Formula (FormulaState(..))
import App.Components.Table.Handler (handleAction)
import App.Components.Table.Models (Action(..), AppState)
import App.Components.Table.Renderer (render)
import App.Components.Table.Selection (MultiSelection(..), SelectionState(..))
import Data.HashMap as HashMap
import Halogen as H

component
  :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall a. a -> AppState
initialState = const
  { selectedCell: { column: mkColumn 'A', row: mkRow 1 }
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
  , rows: bottom .. mkRow 100
  , multiSelection: NoSelection
  , selectionState: NotStartedSelection
  , draggedHeader: Nothing
  , fnsMap: HashMap.empty
  , operatorsMap: HashMap.empty
  , aliasedModulesMap: HashMap.empty
  , importedModulesMap: HashMap.empty
  }
