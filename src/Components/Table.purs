module App.Components.Table where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (CellValue(..), Column(..), Row(..))
import App.Components.Table.Formula (FormulaState(..))
import App.Components.Table.Handler (handleAction)
import App.Components.Table.Models (Action(..), AppState)
import App.Components.Table.Renderer (render)
import App.Components.Table.Selection (MultiSelection(..), SelectionState(..))
import Data.Map as Map
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
  { selectedCell: { column: Column 'A', row: Row 1 }
  , formulaCell: { column: Column 'A', row: Row 1 }
  , activeFormula: false
  , activeInput: false
  , formulaState: UnknownFormula
  , tableData: Map.fromFoldable
      [ { column: Column 'A', row: Row 1 } /\ IntVal 1
      , { column: Column 'B', row: Row 1 } /\ IntVal 2
      , { column: Column 'C', row: Row 1 } /\ IntVal 3
      , { column: Column 'D', row: Row 1 } /\ IntVal 4
      ]
  , tableDependencies: Map.empty
  , tableFormulas: Map.empty
  , formulaCache: Map.empty
  , columns: Column <$> 'A' .. 'Z'
  , rows: Row <$> 1 .. 100
  , multiSelection: NoSelection
  , selectionState: NotStartedSelection
  , draggedHeader: Nothing
  , fnsMap: Map.empty
  , operatorsMap: Map.empty
  , aliasedModulesMap: Map.empty
  , importedModulesMap: Map.empty
  }
