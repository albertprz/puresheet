module App.Evaluator.Formula where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue, buildCell)
import App.Components.Table.Models (AppState)
import App.Evaluator.Common (LocalFormulaCtx, nonEmptyCellValue)
import App.Evaluator.Errors (EvalError(..), SerializationError(..))
import App.Evaluator.Expression (evalExpr)
import App.Evaluator.Object (objectToCellValues)
import App.SyntaxTree.FnDef (FnBody, Object)
import Control.Monad.Except (runExceptT)
import Data.Map as Map
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree.Zipper (fromTree)
import Matrix as Matrix

evalFormula
  :: AppState
  -> FnBody
  -> Either EvalError
       { result :: Map Cell CellValue
       , affectedCells :: NonEmptySet Cell
       }
evalFormula appState body = do
  objectResult <- evalExprInApp appState body
  note (SerializationError' CellValueSerializationError)
    $
      ( \result -> { result, affectedCells: _ } <$>
          (NonEmptySet.fromSet $ Map.keys result)
      )
    =<< toCellMap
    <$> join (partialMaybe objectToCellValues objectResult)
  where
  { columns, rows, selectedCell: { column, row } } = appState
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

evalExprInApp
  :: AppState -> FnBody -> Either EvalError Object
evalExprInApp appState =
  evalExprInCtx
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    , argsMap: Map.empty
    , scope: zero
    , scopeLoc: fromTree $ mkLeaf zero
    }

evalExprInCtx :: LocalFormulaCtx -> FnBody -> Either EvalError Object
evalExprInCtx formulaCtx exprBody = do
  evalState (runExceptT (evalExpr exprBody)) formulaCtx
