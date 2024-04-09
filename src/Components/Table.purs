module App.Components.Table where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (CellValue(..), mkColumn, mkRow)
import App.Components.Table.Formula (FormulaState(..))
import App.Components.Table.Handler (handleAction)
import App.Components.Table.Models (TableAction(..), TableState)
import App.Components.Table.Renderer (render)
import App.Components.Table.Selection (MultiSelection(..), SelectionState(..))
import Data.HashMap as HashMap
import Halogen (Component, defaultEval, mkComponent, mkEval)

component :: forall q m. MonadAff m => Component q Unit Unit m
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall a. a -> TableState
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
  , fnsMap: HashMap.empty
  , operatorsMap: HashMap.empty
  , aliasedModulesMap: HashMap.empty
  , importedModulesMap: HashMap.empty
  , rows: mkRow <$> (0 .. 100)
  , multiSelection: NoSelection
  , selectionState: NotStartedSelection
  , draggedHeader: Nothing
  }
