module Interpreter.ExpressionSpec where

import TestPrelude

import App.Components.Table.Cell (Column(..), Row(..))
import App.Evaluator.Builtins as Builtins
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Errors (EvalError(..), LexicalError(..), MatchError(..), TypeError(..))
import App.Interpreter.Expression (RunError(..))
import App.Interpreter.Expression as Interpreter
import App.SyntaxTree.Common (QVar(..), QVarOp(..), Var(..), VarOp(..), preludeModule)
import App.SyntaxTree.FnDef (Object(..))
import Data.Map as Map
import Data.Tree.Zipper (fromTree)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Interpreter.Expression" do

  describe "runExpr" do

    describe "works for recursive functions" do

      it "Fibonacci" $
        runExpr
          """
          fib(10) where {
              | fib(x) = switch (x) {
                  | 0 => 0
                  | 1 => 1
                  | _ => fib (x - 1) + fib (x - 2)
              }
          }
          """ `shouldEqual` pure
          (IntObj 55)

      it "Map" $
        runExpr
          """
          map(square, [1 .. 4]) where {
              | square (x) = x * x
              | map (f, xs) = switch (xs) {
                  | [] => []
                  | xs => map(f, init (xs)) :+ f (last (xs))
              }
          }
          """ `shouldEqual` pure
          (ArrayObj (IntObj <$> [ 1, 4, 9, 16 ]))

    describe "works for partially applied functions" do

      it "Curried function" $
        runExpr
          """
          mult2(10) where {
              | mult2 = mult(2)
          }
          """ `shouldEqual` pure
          (IntObj 20)

      it "Operator section" $
        runExpr
          """
          mult2(10) where {
              | mult2 = (_ * 2)
          }
          """ `shouldEqual` pure
          (IntObj 20)

    describe "applies precedence for infix operators" do
      it "Infix arithmetic expression" do
        runExpr
          """
          3 / 4 - 6 + 8 * 9
          """ `shouldEqual` pure
          (FloatObj 66.75)

    describe "looks for bindings in lexical scope" do

      it "Successful lookup" $
        runExpr
          """
          f(5) where {
              | f(x) = g(x + h(x) * z) where {
                  | g(x) = 3 + x
                  | h(y) = 2 * x
              }
              | z = 2
          }
          """ `shouldEqual` pure
          (IntObj 28)

      it "Unaccesible nested binding" $
        runExpr
          """
          g(1) where {
              | f(x) = g(x) where {
                  | g(x) = 3
              }
          }
          """ `shouldEqual` evalError
          (LexicalError' $ UnknownValue $ mkQVar "g")

      it "Unbound value" $
        runExpr
          """
          y
          """ `shouldEqual` evalError
          (LexicalError' $ UnknownValue $ mkQVar "y")

      it "Unbound operator" $
        runExpr
          """
          1 -+ 2
          """ `shouldEqual` evalError
          (LexicalError' $ UnknownOperator $ mkQVarOp "-+")

    describe "evaluates pattern and guard matches" do

      it "Succesful pattern match" $
        runExpr
          """
          switch ([1, 2, 3, 4, 5, 6]) {
              | [x, y, ... , z] => [x, y, z]
          }
          """ `shouldEqual` pure
          (ArrayObj $ IntObj <$> [ 1, 2, 6 ])

      it "Succesful guard match" $
        runExpr
          """
          cond {
              ? length(xs) == 0 => "empty"
              ? length(xs) == 1 => "one elem"
              ? length(xs) == 2 => "two elems"
              ? otherwise       => "any number of elems"
          } where {
              | xs = [1, 2]
          }
          """ `shouldEqual` pure
          (StringObj "two elems")

      it "Complex guard match" $
        runExpr
          """
          cond {
              ? [x, y] <- xs, x > y => x
              ? [x, y] <- xs, y > x => y
              ? otherwise           => 0
          } where {
              | xs = [1, 2]
          }
          """ `shouldEqual` pure (IntObj 2)

      it "Guarded pattern match" $
        runExpr
          """
          switch (xs) {
              | [x]                 => x
              | [x, y] ? x % 2 == 0 => x * x
                       ? y % 2 == 0 => y * y
                       ? otherwise  => 0
          } where {
              | xs = [3, 4]
          }
          """ `shouldEqual` pure (IntObj 16)

      it "Non exhaustive pattern match" $
        runExpr
          """
          switch ([1]) {
              | [] => []
          }          """ `shouldEqual` evalError
          (MatchError' NonExhaustiveMatch)

      it "Non boolean guard" $
        runExpr
          """
          cond {
              ? 0 => true
          }
          """ `shouldEqual` evalError
          (MatchError' InvalidGuard)

    describe "raises type errors on invalid function calls" do

      it "Too many arguments" $
        runExpr
          """
          add(1, 2, 3)
          """ `shouldEqual` evalError
          (TypeError' $ TooManyArguments 3)

      it "Not a function" $
        runExpr
          """
          f(1) where {
              | f = []
          }
          """ `shouldEqual` evalError
          (TypeError' $ NotAFunction $ ArrayObj [])

      it "Invalid argument types" $
        runExpr
          """
          "hello" - "world"
          """ `shouldEqual` evalError
          ( TypeError' $ InvalidArgumentTypes
              (StringObj <$> [ "hello", "world" ])
          )
      it "Invalid cell array range" $
        runExpr
          """
          [| A1 .. B2 |]
          """ `shouldEqual` evalError
          ( TypeError' $ InvalidCellArrayRange
              { column: Column 'A', row: Row 1 }
              { column: Column 'B', row: Row 2 }
          )

mkQVar :: String -> QVar
mkQVar = QVar Nothing <<< Var

mkQVarOp :: String -> QVarOp
mkQVarOp = QVarOp Nothing <<< VarOp

runExpr :: String -> Either RunError Object
runExpr = Interpreter.runExpr formulaCtx

evalError :: forall a. EvalError -> Either RunError a
evalError = Left <<< EvalError'

formulaCtx :: LocalFormulaCtx
formulaCtx =
  { tableData: Map.empty
  , fnsMap: Map.empty
  , operatorsMap: Builtins.operatorsMap
  , aliasedModulesMap: Map.empty
  , importedModulesMap: Map.empty
  , localFnsMap: Map.empty
  , argsMap: Map.empty
  , module': preludeModule
  , scope: zero
  , scopeLoc: fromTree $ mkLeaf zero
  }
