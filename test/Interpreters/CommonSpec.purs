module Interpreters.CommonSpec where

import TestPrelude

import App.Interpreters.Builtins as Builtins
import App.Interpreters.Common (LocalFormulaCtx, evalExpr, mkLeaf)
import App.Interpreters.Errors (EvalError)
import App.Parsers.FnDef (fnBody)
import App.SyntaxTrees.FnDef (FnBody, Object(..))
import Bookhound.Parser (ParseError, runParser)
import Control.Monad.Except (runExceptT)
import Data.Map as Map
import Data.Tree.Zipper (fromTree)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Interpreters.Common" $ do

  describe "evalExpr" $ do

    describe "evaluates recursive functions:" $ do

      it "Fibonacci" $
        runFnBody
          """
          fib(10) where {
              | fib(x) = switch (x) {
                  | 0 -> 0
                  | 1 -> 1
                  | _ -> fib (x - 1) + fib (x - 2)
              }
          }
          """ `shouldEqual` pure (IntObj 55)

      it "Map" $
        runFnBody
          """
          map(square, [1 .. 4]) where {
              | square (x) = x * x
              | map (f, xs) = switch (xs) {
                  | [] -> []
                  | xs -> f (head (xs)) +: map(f, tail (xs))
              }
          }
          """ `shouldEqual` pure (ListObj (IntObj <$> [ 1, 4, 9, 16 ]))

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
