module Evaluator.ExpressionSpec where

import TestPrelude

import App.Evaluator.Builtins as Builtins
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Errors (EvalError(..), LexicalError(..), MatchError(..), TypeError(..))
import App.Evaluator.Expression (evalExpr)
import App.Parser.FnDef (fnBody)
import App.SyntaxTree.Common (Var(..), VarOp(..))
import App.SyntaxTree.FnDef (FnBody, Object(..))
import Bookhound.Parsers (ParseError, runParser)
import Control.Monad.Except (runExceptT)
import Data.Map as Map
import Data.Tree.Zipper (fromTree)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Evaluator.Expression" do

  describe "evalExpr" do

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
          (LexicalError' $ UnknownValue $ Var "g")

      it "Unbound value" $
        runExpr
          """
          y
          """ `shouldEqual` evalError
          (LexicalError' $ UnknownValue $ Var "y")

      it "Unbound operator" $
        runExpr
          """
          1 -+ 2
          """ `shouldEqual` evalError
          (LexicalError' $ UnknownOperator $ VarOp "-+")

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

runExpr :: String -> Either RunError Object
runExpr = (lmap EvalError' <<< evalExprInCtx)
  <=< (lmap ParseErrors' <<< parseExpr)

parseExpr :: String -> Either (Array ParseError) FnBody
parseExpr exprText = runParser fnBody exprText

evalExprInCtx :: FnBody -> Either EvalError Object
evalExprInCtx exprBody = do
  evalState (runExceptT (evalExpr exprBody)) formulaCtx

formulaCtx :: LocalFormulaCtx
formulaCtx =
  { tableData: Map.empty
  , fnsMap: Map.empty
  , argsMap: Map.empty
  , operatorsMap: Builtins.operatorsMap
  , scope: zero
  , scopeLoc: fromTree $ mkLeaf zero
  }

evalError :: forall t80. EvalError -> Either RunError t80
evalError = Left <<< EvalError'

data RunError
  = EvalError' EvalError
  | ParseErrors' (Array ParseError)

derive instance Eq RunError

instance Show RunError where
  show (EvalError' x) = show x
  show (ParseErrors' x) = show x
