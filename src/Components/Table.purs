module App.Components.Table where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (CellValue(..), Column(..), Row(..))
import App.Components.Table.Models (State)
import App.Components.Table.Render (render)
import App.Components.Table.Handler (handleAction)
import Data.Map as Map
import Halogen as H

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall a. a -> State
initialState = const
  { selectedCell: { column: Column 'A', row: Row 1 }
  , activeInput: false
  , tableData: Map.fromFoldable
      [ { column: Column 'A', row: Row 1 } /\ StringVal "value"
      , { column: Column 'D', row: Row 1 } /\ StringVal "another value"
      ]
  , columns: Column <$> 'A' .. 'Z'
  , rows: Row <$> 1 .. 30
  }

