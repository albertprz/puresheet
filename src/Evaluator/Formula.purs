module App.Evaluator.Formula where

import FatPrelude

import App.Components.Spreadsheet.Cell (Cell, CellValue)
import App.Evaluator.Common (LocalFormulaCtx, nonEmptyCellValue)
import App.Evaluator.Errors (EvalError(..), SerializationError(..))
import App.Evaluator.Expression (evalExpr)
import App.Evaluator.Object (objectToCellValues)
import App.SyntaxTree.FnDef (FnBody, Object)
import Data.HashMap as HashMap
import Data.Set.NonEmpty as NonEmptySet
import Matrix as Matrix

evalFormula
  :: LocalFormulaCtx
  -> Cell
  -> FnBody
  -> Either EvalError
       { result :: HashMap Cell CellValue
       , affectedCells :: NonEmptySet Cell
       }
evalFormula ctx { column, row } body = do
  objectResult <- evalExprInCtx ctx body
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
        ( getInBoundedRange (column + wrap x) /\
            getInBoundedRange (row + wrap y)
        )

evalExprInCtx :: LocalFormulaCtx -> FnBody -> Either EvalError Object
evalExprInCtx formulaCtx exprBody = do
  flip evalState formulaCtx $ runExceptT $ evalExpr exprBody
