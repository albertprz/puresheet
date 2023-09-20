module App.Interpreters.Formula where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue, buildCell)
import App.Components.Table.Models (AppState)
import App.Interpreters.Common (nonEmptyCellValue)
import App.Interpreters.Errors (EvalError(..), SerializationError(..))
import App.Interpreters.Expression (evalExpr)
import App.Interpreters.Object (objectToCellValues)
import App.SyntaxTrees.FnDef (FnBody, Object)
import Control.Monad.Except (ExceptT, except, runExceptT)
import Data.Map as Map
import Data.Tree.Zipper (fromTree)
import Matrix as Matrix

evalFormula
  :: forall m
   . MonadState AppState m
  => Cell
  -> FnBody
  -> ExceptT EvalError m (Map Cell CellValue)
evalFormula { column, row } body = do
  { columns, rows } <- get
  obj <- evalExprInApp body
  let
    toCellMap cellMatrix =
      Map.filter nonEmptyCellValue
        $ Map.fromFoldable
        $ filterMap toCellPair
        $ Matrix.toIndexedArray cellMatrix
    toCellPair { x, y, value } =
      (_ /\ value) <<< uncurry buildCell <$>
        bisequence
          ( getElemSat (_ + x) columns column /\
              getElemSat (_ + y) rows row
          )
  except $ note (SerializationError' CellValueSerializationError)
    $ toCellMap
    <$> join (partialMaybe objectToCellValues $ obj)

evalExprInApp
  :: forall m. MonadState AppState m => FnBody -> ExceptT EvalError m Object
evalExprInApp expr = do
  appState <- get
  except $ evalState (runExceptT $ evalExpr expr)
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    , argsMap: Map.empty
    , scope: zero
    , scopeLoc: fromTree $ mkLeaf zero
    }
