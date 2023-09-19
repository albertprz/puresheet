module Interpreters.CommonSpec where

import TestPrelude

import App.Interpreters.Builtins as Builtins
import App.Interpreters.Common (LocalFormulaCtx, evalExpr, mkLeaf)
import App.Interpreters.Errors (EvalError(..), LexicalError(..))
import App.Parsers.FnDef (fnBody)
import App.SyntaxTrees.Common (Var(..), VarOp(..))
import App.SyntaxTrees.FnDef (FnBody, Object(..))
import Bookhound.Parser (ParseError, runParser)
import Control.Monad.Except (runExceptT)
import Data.Map as Map
import Data.Tree.Zipper (fromTree)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Interpreters.Common" do

  describe "evalExpr" do

    describe "works for recursive functions" do

      it "Fibonacci" $
        runFnBody
          """
          fib(10) where {
              | fib(x) = switch (x) {
                  | 0 => 0
                  | 1 => 1
                  | _ => fib (x - 1) + fib (x - 2)
              }
          }
          """ `shouldEqual` pure (IntObj 55)

      it "Map" $
        runFnBody
          """
          map(square, [1 .. 4]) where {
              | square (x) = x * x
              | map (f, xs) = switch (xs) {
                  | [] => []
                  | xs => f (head (xs)) +: map(f, tail (xs))
              }
          }
          """ `shouldEqual` pure (ListObj (IntObj <$> [ 1, 4, 9, 16 ]))

    describe "looks for bindings in lexical scope" do

      it "Successful lookup" $
        runFnBody
          """
          f(5) where {
              | f(x) = g(x + h(x) * z) where {
                  | g(x) = 3 + x
                  | h(y) = 2 * x
              }
              | z = 2
          }
          """ `shouldEqual` pure (IntObj 28)

      it "Unaccesible nested binding" $
        runFnBody
          """
          g(1) where {
              | f(x) = g(x) where {
                  | g(x) = 3
              }
          }
          """ `shouldEqual` evalError (LexicalError' $ UnknownValue $ Var "g")

      it "Unbound value" $
        runFnBody
          """
          y
          """ `shouldEqual` evalError (LexicalError' $ UnknownValue $ Var "y")

      it "Unbound operator" $
        runFnBody
          """
          1 -+ 2
          """ `shouldEqual` evalError (LexicalError' $ UnknownOperator $ VarOp "-+")


    describe "evaluates pattern and guard matches" do

      it "Succesful pattern" $
        runFnBody
          """
          switch ([1, 2, 3, 4, 5, 6]) {
              | [x, y, ... , z] => [x, y, z]
          }
          """ `shouldEqual` pure (ListObj $ IntObj <$> [1, 2, 6])

      it "Succesful guard" $
        runFnBody
          """
          cond {
              ? length(xs) == 0 => "empty"
              ? length(xs) == 1 => "one elem"
              ? length(xs) == 2 => "two elems"
              ? true            => "any number of elems"
          } where {
              | xs = [1, 2]
          }
          """ `shouldEqual` pure (StringObj "two elems")


runFnBody :: String -> Either RunError Object
runFnBody = (lmap EvalError' <<< evalFnBody)
  <=< (lmap ParseErrors' <<< parseFnBody)

parseFnBody :: String -> Either (Array ParseError) FnBody
parseFnBody exprText = runParser fnBody exprText

evalFnBody :: FnBody -> Either EvalError Object
evalFnBody exprBody = do
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
