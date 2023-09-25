module App.Interpreter.Formula where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.Components.Table.Formula (FormulaId, getDependencies)
import App.Components.Table.Models (AppState)
import App.Evaluator.Formula (evalFormula)
import App.Interpreter.Expression (RunError(..), run)
import App.SyntaxTree.FnDef (FnBody(..), FnDef(..))
import Data.Set as Set
import Data.Set.NonEmpty (toSet)
import Data.Tree (Forest)

type FormulaResult =
  { result :: Map Cell CellValue
  , affectedCells :: NonEmptySet Cell
  , formulaCells :: Set Cell
  , cellDeps :: Forest FormulaId
  }

runFormula :: AppState -> Cell -> String -> Either RunError FormulaResult
runFormula appState cell =
  (lmap DependencyError' <<< depsFn) <=< (run evalFn)
  where
  depsFn { result, affectedCells, formulaCells } =
    { result, affectedCells, formulaCells, cellDeps: _ }
      <$> getDependencies appState affectedCells formulaCells
  evalFn body = evalFnHelper body
    <$> evalFormula appState cell body
  evalFnHelper body { result, affectedCells } =
    { result
    , affectedCells
    , formulaCells: Set.filter (_ `notElem` toSet affectedCells)
        (extractCells body)
    }

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
