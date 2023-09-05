module App.Components.Table where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (CellValue(..), Column(..), MultiSelection(..), Row(..), SelectionState(..))
import App.Components.Table.Handler (handleAction)
import App.Components.Table.Models (Action(..), AppState)
import App.Components.Table.Renderer (render)
import App.Interpreters.Builtins (builtinFnsMap, operatorsMap)
import Data.Map as Map
import Halogen as H

component :: forall q i o m. MonadAff m => H.Component q i o m
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
  , activeInput: false
  , tableData: Map.fromFoldable
      [ { column: Column 'A', row: Row 1 } /\ StringVal "value"
      , { column: Column 'D', row: Row 1 } /\ StringVal "another value"
      ]
  , columns: Column <$> 'A' .. 'Z'
  , rows: Row <$> 1 .. 100
  , multiSelection: NoSelection
  , selectionState: NotStartedSelection
  , draggedHeader: Nothing
  , formulaCtx:
      { fnsMap: Map.empty
      , builtinFnsMap
      , operatorsMap
      }
  }
