module App.Interpreter.Formula where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.Components.Table.Formula (FormulaId, getDependencies)
import App.Components.Table.Models (AppState)
import App.Evaluator.Formula (evalFormula)
import App.Interpreter.Expression (RunError(..), run)
import App.SyntaxTree.FnDef (CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
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

extractCells (CondExpr conds) =
  foldMap extractCellsFromGuardedBody conds

extractCells (SwitchExpr matchee cases) =
  extractCells matchee <> foldMap extractCellsFromCaseBinding cases

extractCells (ArrayRange x y) =
  extractCells x <> extractCells y

extractCells
  ( CellMatrixRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  ) = Set.fromFoldable do
  row <- toArray $ rowX .. rowY
  column <- toArray $ colX .. colY
  pure { column, row }

extractCells (Array' array) =
  foldMap extractCells array

extractCells (Cell' cell) = Set.singleton cell

extractCells (FnVar' _) = mempty

extractCells (FnOp _) = mempty

extractCells (CellValue' _) = mempty

extractCells (Object' _) = mempty

extractCellsFromCaseBinding :: CaseBinding -> Set Cell
extractCellsFromCaseBinding (CaseBinding _ maybeGuardedBody) =
  extractCellsFromMaybeGuardedBody maybeGuardedBody

extractCellsFromMaybeGuardedBody :: MaybeGuardedFnBody -> Set Cell
extractCellsFromMaybeGuardedBody (Guarded guardedBodies) =
  foldMap extractCellsFromGuardedBody guardedBodies
extractCellsFromMaybeGuardedBody (Standard body) =
  extractCells body

extractCellsFromGuardedBody :: GuardedFnBody -> Set Cell
extractCellsFromGuardedBody (GuardedFnBody guard body) =
  extractCellsFromGuard guard <> extractCells body

extractCellsFromGuard :: Guard -> Set Cell
extractCellsFromGuard (Guard patternGuards) =
  foldMap extractCellsFromPatternGuard patternGuards
extractCellsFromGuard Otherwise = mempty

extractCellsFromPatternGuard :: PatternGuard -> Set Cell
extractCellsFromPatternGuard (PatternGuard _ body) =
  extractCells body
extractCellsFromPatternGuard (SimpleGuard body) =
  extractCells body
