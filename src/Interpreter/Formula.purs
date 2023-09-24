module App.Interpreter.Formula where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.Components.Table.Models (AppState)
import App.Evaluator.Formula (evalFormula)
import App.Interpreter.Expression (RunError, run)
import App.SyntaxTree.FnDef (FnBody(..), FnDef(..))
import Data.Set as Set

runFormula
  :: AppState -> String -> Either RunError ((Map Cell CellValue) /\ Set Cell)
runFormula appState =
  run
    ( \body -> (\x -> (x /\ extractCells body))
        <$> evalFormula appState body
    )

extractCells :: FnBody -> Set Cell
extractCells (FnApply fnExpr args) =
  extractCells fnExpr <> foldMap extractCells args

extractCells (InfixFnApply _ args) =
  foldMap extractCells args

extractCells (LeftOpSection _ body) =
  extractCells body

extractCells (RightOpSection body _) =
  extractCells body

extractCells (WhereExpr fnBody bindings) =
  extractCells fnBody <>
    foldMap (extractCells <<< (\(FnDef _ _ body) -> body)) bindings

-- extractCells (CondExpr conds) =
--   foldMap extractCells conds

-- extractCells (SwitchExpr matchee cases) =
--   extractCells matchee <> foldMap extractCells cases

extractCells (ListRange x y) =
  extractCells x <> extractCells y

extractCells
  ( MatrixRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  ) = Set.fromFoldable do
  row <- toArray $ rowX .. rowY
  column <- toArray $ colX .. colY
  pure { column, row }

extractCells (List list) =
  foldMap extractCells list

extractCells (FnVar' _) = Set.empty

extractCells (FnOp _) = Set.empty

extractCells (Cell' cell) = Set.singleton cell

extractCells (CellValue' _) = Set.empty

extractCells (Object' _) = Set.empty

extractCells _ = Set.empty
