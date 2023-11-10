module App.Evaluator.Formula where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.Components.Table.Models (AppState)
import App.Evaluator.Common (LocalFormulaCtx, nonEmptyCellValue)
import App.Evaluator.Errors (EvalError(..), SerializationError(..))
import App.Evaluator.Expression (evalExpr)
import App.Evaluator.Object (objectToCellValues)
import App.SyntaxTree.Common (preludeModule)
import App.SyntaxTree.FnDef (FnBody, Object)
import Data.HashMap as HashMap
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree.Zipper (fromTree)
import Matrix as Matrix

evalFormula
  :: AppState
  -> Cell
  -> FnBody
  -> Either EvalError
       { result :: HashMap Cell CellValue
       , affectedCells :: NonEmptySet Cell
       }
evalFormula appState { column, row } body = do
  objectResult <- evalExprInApp appState body
  note (SerializationError' CellValueSerializationError)
    $
      ( \result -> { result, affectedCells: _ } <$>
          (NonEmptySet.fromFoldable $ HashMap.keys result)
      )
    =<< toCellMap
    <$> join (partialMaybe objectToCellValues objectResult)
  where
  toCellMap cellMatrix =
    HashMap.filter nonEmptyCellValue
      $ HashMap.fromArray
      $ filterMap toCellPair
      $ Matrix.toIndexedArray cellMatrix
  toCellPair { x, y, value } =
    (_ /\ value) <<< uncurry { column: _, row: _ } <$>
      bisequence
        ( getElemSat (column + wrap x) /\
            getElemSat (row + wrap y)
        )

evalExprInApp
  :: AppState -> FnBody -> Either EvalError Object
evalExprInApp appState = evalExprInCtx $ mkLocalContext appState

evalExprInCtx :: LocalFormulaCtx -> FnBody -> Either EvalError Object
evalExprInCtx formulaCtx exprBody = do
  flip evalState formulaCtx $ runExceptT $ evalExpr exprBody

mkLocalContext :: AppState -> LocalFormulaCtx
mkLocalContext appState =
  { tableData: appState.tableData
  , fnsMap: appState.fnsMap
  , operatorsMap: appState.operatorsMap
  , aliasedModulesMap: appState.aliasedModulesMap
  , importedModulesMap: appState.importedModulesMap
  , localFnsMap: HashMap.empty
  , argsMap: HashMap.empty
  , module': preludeModule
  , scope: zero
  , scopeLoc: fromTree $ mkLeaf zero
  , lambdaCount: zero
  }
