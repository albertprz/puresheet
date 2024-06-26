module Interpreter.ExpressionSpec where

import TestPrelude

import App.AppStore (emptyStore, mkLocalContext)
import App.Components.Spreadsheet.Cell (Column(..), Row(..))
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Errors (EvalError(..), LexicalError(..), MatchError(..), TypeError(..))
import App.Interpreter.Expression (RunError(..))
import App.Interpreter.Expression as Interpreter
import App.Interpreter.Module (reloadModule)
import App.Lib.Prelude (prelude)
import App.SyntaxTree.Common (QVar(..), QVarOp(..), Var(..), VarOp(..))
import App.SyntaxTree.FnDef (Object(..))
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Interpreter.Expression" do

  describe "runExpr" do

    describe "works for recursive functions" do

      it "Non tail calls (can stack overflow)" $
        runExpr
          """
          fib (10) where {
              | fib (x) = switch (x) {
                  | 0 => 0
                  | 1 => 1
                  | _ => fib (x - 1) + fib (x - 2)
              }
          }
          """ `shouldEqual` pure
          (IntObj 55)

      it "Tail calls (cannot stack overflow)" $
        runExpr
          """
          last (myMap (x -> x * x, [1 .. 5000], [])) where {
              | myMap (f, xs, ys) = switch (xs) {
                  | [] => ys
                  | [ xs @ ... , x ] => recur (f, xs, f (x) +: ys)
              }
          }
          """ `shouldEqual` pure
          (IntObj 25000000)

    describe "works for partially applied functions" do

      it "Curried function" $
        runExpr
          """
          mult2 (10) where {
              | mult2 = mult (2)
          }
          """ `shouldEqual` pure
          (IntObj 20)

      it "Operator section" $
        runExpr
          """
          mult2 (10) where {
              | mult2 = _ * 2
          }
          """ `shouldEqual` pure
          (IntObj 20)

      it "Higher order curried function" $
        runExpr
          """
          f (5) where {
              | f = myCompose (_ * 2, _ + 3)
              | myCompose (f, g) = h where {
                  | h (x) = f (g (x))
              }
          }
          """ `shouldEqual` pure
          (IntObj 16)

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
          f (5) where {
              | f(x) = g(x + h(x) * z) where {
                  | g (x) = 3 + x
                  | h (y) = 2 * x
              }
              | z = 2
          }
          """ `shouldEqual` pure
          (IntObj 28)

      it "Unaccesible nested binding" $
        runExpr
          """
          g (1) where {
              | f (x) = g (x) where {
                  | g (x) = 3
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

    describe "evaluates switch expressions" do

      it "Successful pattern match" $
        runExpr
          """
          switch ([1, 2, 3, 4, 5, 6]) {
              | [x, y, ... , z] => [x, y, z]
          }
          """ `shouldEqual` pure
          (ArrayObj $ IntObj <$> [ 1, 2, 6 ])

      it "Guarded patterns" $
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

    describe "evaluates cond expressions" do

      it "Successful guard match" $
        runExpr
          """
          cond {
              ? length (xs) == 0 => "empty"
              ? length (xs) == 1 => "one elem"
              ? length (xs) == 2 => "two elems"
              ? otherwise        => "more elems"
          } where {
              | xs = [1, 2]
          }
          """ `shouldEqual` pure
          (StringObj "two elems")

      it "Complex guards" $
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

      it "Non exhaustive guard" $
        runExpr
          """
          cond {
              ? 1 == 2 => "fst option"
              ? 2 == 3 => "snd option"
          }
          """ `shouldEqual` evalError
          (MatchError' NonExhaustiveGuard)

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
          add (1, 2, 3)
          """ `shouldEqual` evalError
          (TypeError' $ TooManyArguments 3)

      it "Not a function" $
        runExpr
          """
          f (1) where {
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
              { column: Column $ fromUpper 'A', row: Row 1 }
              { column: Column $ fromUpper 'B', row: Row 2 }
          )

    describe "evaluates top level functions & operators" do

      it "Map filter pipeline" $
        runExpr
          """
          [1, 3, 5] |map> (_ * 2) |filter> (_ > 7)
          """ `shouldEqual`
          (pure $ ArrayObj [ IntObj 10 ])

      it "Map filter reverse pipeline" $
        runExpr
          """
          (_ > 7) <filter| (_ * 2) <map| [1, 3, 5]
          """ `shouldEqual`
          (pure $ ArrayObj [ IntObj 10 ])

runExpr :: String -> Either RunError Object
runExpr = Interpreter.runExpr formulaCtx

mkQVar :: String -> QVar
mkQVar = QVar Nothing <<< Var

mkQVarOp :: String -> QVarOp
mkQVarOp = QVarOp Nothing <<< VarOp

evalError :: forall a. EvalError -> Either RunError a
evalError = Left <<< EvalError'

formulaCtx :: LocalFormulaCtx
formulaCtx =
  execState loadPrelude $ mkLocalContext emptyStore
  where
  loadPrelude =
    unsafeFromJust <<< hush <$> reloadModule prelude
