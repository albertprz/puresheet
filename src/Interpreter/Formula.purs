module App.Interpreter.Formula (runFormula) where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.Components.Table.Formula (FormulaId, getDependencies)
import App.Components.Table.Models (TableState)
import App.Evaluator.Formula (evalFormula)
import App.Interpreter.Expression (RunError(..), run)
import App.SyntaxTree.FnDef (CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
import App.Utils.Set (fromUnfoldable) as Set
import Data.Set (filter, singleton) as Set
import Data.Set.NonEmpty (toSet)
import Data.Tree (Forest)

type FormulaResult =
  { result :: HashMap Cell CellValue
  , affectedCells :: NonEmptySet Cell
  , formulaCells :: Set Cell
  , cellDeps :: Forest FormulaId
  }

runFormula :: TableState -> Cell -> String -> Either RunError FormulaResult
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
    , formulaCells: Set.filter (flip notElem $ toSet affectedCells)
        (extractCells body)
    }

extractCells :: FnBody -> Set Cell
extractCells (FnApply fnExpr args) =
  extractCells fnExpr <> foldMap extractCells args

extractCells (LambdaFn _ body) =
  extractCells body

extractCells (InfixFnApply _ args) =
  foldMap extractCells args

extractCells (LeftOpSection _ body) =
  extractCells body

extractCells (RightOpSection body _) =
  extractCells body

extractCells (WhereExpr fnBody bindings) =
  extractCells fnBody <>
    foldMap (extractCells <<< (\(FnDef _ _ _ body) -> body)) bindings

extractCells (CondExpr conds) =
  foldMap extractCellsFromGuardedBody conds

extractCells (SwitchExpr matchee cases) =
  extractCells matchee <> foldMap extractCellsFromCaseBinding cases

extractCells
  ( CellMatrixRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  ) = Set.fromUnfoldable do
  row <- toArray (rowX .. rowY)
  column <- toArray (colX .. colY)
  pure { column, row }

extractCells
  ( CellArrayRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  )
  | rowX == rowY =
      Set.fromUnfoldable $ { column: _, row: rowX } <$> toArray (colX .. colY)
  | colX == colY =
      Set.fromUnfoldable $ { column: colX, row: _ } <$> toArray (rowX .. rowY)
  | otherwise = mempty

extractCells (ArrayRange x y) =
  extractCells x <> extractCells y

extractCells (Array' array) =
  foldMap extractCells array

extractCells (Cell' cell) = Set.singleton cell

extractCells (FnVar _) = mempty

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
