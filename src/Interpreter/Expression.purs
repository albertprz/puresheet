module App.Interpreter.Expression where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.Components.Table.Models (AppState)
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Errors (EvalError)
import App.Evaluator.Formula (evalExprInCtx, evalFormula)
import App.Parser.FnDef (fnBody)
import App.SyntaxTree.FnDef (FnBody, Object)
import Bookhound.Parser (ParseError, runParser)

runFormula :: AppState -> String -> Either RunError (Map Cell CellValue)
runFormula = run <<< evalFormula

runExpr :: LocalFormulaCtx -> String -> Either RunError Object
runExpr = run <<< evalExprInCtx

run :: forall a. (FnBody -> Either EvalError a) -> String -> Either RunError a
run evalFn =
  (lmap EvalError' <<< evalFn) <=< (lmap ParseErrors' <<< runParser fnBody)

data RunError
  = EvalError' EvalError
  | ParseErrors' (Array ParseError)

derive instance Eq RunError

instance Show RunError where
  show (EvalError' x) = show x
  show (ParseErrors' x) = show x
