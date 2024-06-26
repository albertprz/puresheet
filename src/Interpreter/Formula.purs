module App.Interpreter.Formula (runFormula, parseAndRunFormula) where

import FatPrelude

import App.AppStore (Store, mkLocalContext)
import App.Components.Spreadsheet.Cell (Cell, CellValue)
import App.Components.Spreadsheet.Formula (FormulaId, getDependencies)
import App.Evaluator.Formula (evalFormula)
import App.Interpreter.Expression (RunError(..))
import App.Parser.FnDef as Parser
import App.SyntaxTree.FnDef (CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
import App.Utils.Set (fromArray) as Set
import Bookhound.Parser (runParser)
import Data.Set (filter, singleton) as Set
import Data.Set.NonEmpty (toSet)
import Data.Tree (Forest)

type FormulaResult =
  { result :: HashMap Cell CellValue
  , affectedCells :: NonEmptySet Cell
  , formulaCells :: Set Cell
  , cellDeps :: Forest FormulaId
  }

runFormula :: Store -> Cell -> FnBody -> Either RunError FormulaResult
runFormula store cell =
  (lmap DependencyError' <<< depsFn) <=< (lmap EvalError' <<< evalFn)
  where
  ctx = mkLocalContext store
  depsFn { result, affectedCells, formulaCells } =
    { result, affectedCells, formulaCells, cellDeps: _ }
      <$> getDependencies store affectedCells formulaCells
  evalFn body = evalFnHelper body
    <$> evalFormula ctx cell body
  evalFnHelper body { result, affectedCells } =
    { result
    , affectedCells
    , formulaCells: Set.filter (flip notElem $ toSet affectedCells)
        (extractCells body)
    }

parseAndRunFormula
  :: Store -> Cell -> String -> Either RunError (FnBody /\ FormulaResult)
parseAndRunFormula store cell formulaText = do
  formulaBody <- lmap ParseErrors' $ runParser Parser.fnBody formulaText
  result <- runFormula store cell formulaBody
  pure (formulaBody /\ result)

extractCells :: FnBody -> Set Cell
extractCells (FnApply fnExpr args) =
  extractCells fnExpr <> foldMap extractCells args

extractCells (Recur args) =
  foldMap extractCells args

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
    foldMap (extractCells <<< (case _ of (FnDef _ _ _ _ body) -> body)) bindings

extractCells (CondExpr conds) =
  foldMap extractCellsFromGuardedBody conds

extractCells (SwitchExpr matchee cases) =
  extractCells matchee <> foldMap extractCellsFromCaseBinding cases

extractCells
  ( CellMatrixRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  ) = Set.fromArray do
  row <- rowX .. rowY
  column <- colX .. colY
  pure { column, row }

extractCells
  ( CellArrayRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  )
  | rowX == rowY =
      Set.fromArray $ { column: _, row: rowX } <$> (colX .. colY)
  | colX == colY =
      Set.fromArray $ { column: colX, row: _ } <$> (rowX .. rowY)
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
