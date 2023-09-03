module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Interpreters.Builtins (builtins, operatorsMap, priorityMap)
import App.Parsers.FnDef (fnDef)
import App.SyntaxTrees.Common (Literal(..), VarOp)
import App.SyntaxTrees.FnDef (FnBody(..), FnVar(..))
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Monad.State (State)
import Data.Filterable (filterMap)
import Data.Map as Map
import Partial.Unsafe (unsafePartial)

type AppM = State Env
type Env = { tableData :: Map Cell CellValue }

reduceExpr :: FnBody -> AppM FnBody
reduceExpr (FnApply (FnVar' (Var' fnName)) args)
  | [ Literal' x, Literal' y ] <- args
  , Just f <- Map.lookup fnName $ unsafePartial builtins = pure $ Literal' $ f x
      y

reduceExpr (FnApply fn args) =
  reduceExpr =<< (FnApply <$> reduceExpr fn <*> traverse reduceExpr args)

reduceExpr (InfixFnApply fns args) =
  reduceExpr $ unsafePartial $ flattenInfixFns (lookupArray fns priorityMap)
    args

reduceExpr (Cell' x) = Literal' <$> fromCell x
reduceExpr x = pure x

flattenInfixFns :: Partial => (Array (VarOp /\ Int)) -> (Array FnBody) -> FnBody
flattenInfixFns [ (fn /\ _) ] args = FnApply (FnVar' $ Var' fnVar) args
  where
  fnVar = unsafeFromJust $ Map.lookup fn operatorsMap

flattenInfixFns fns args
  | Just (fn /\ _) <- maximumBy (compare `on` snd) fns
  , Just idx <- findIndex' ((_ == fn) <<< fst) fns =
      flattenInfixFns newFns newArgs
      where
      fnVar = unsafeFromJust $ Map.lookup fn operatorsMap
      newFns = fold $ deleteAt' idx fns
      redexArgs = slice' idx (idx + 2) args
      newArgs = fold $ deleteAt' (idx + 1) $ fold
        $ updateAt' idx (FnApply (FnVar' $ Var' fnVar) redexArgs) args

flattenInfixFns [ (fn /\ _) ] args = FnApply (FnVar' $ Var' fnVar) args
  where
  fnVar = unsafeFromJust $ Map.lookup fn operatorsMap


fromCell :: Cell -> AppM Literal
fromCell cell = do
  { tableData } <- get
  pure $ maybe (ListLit []) fromCellValue $ Map.lookup cell tableData
  where
  fromCellValue (BoolVal x) = BoolLit x
  fromCellValue (IntVal x) = IntLit x
  fromCellValue (FloatVal x) = FloatLit x
  fromCellValue (StringVal x) = StringLit x

lookupId :: forall k. Ord k => k -> Map k k -> k
lookupId k = fromMaybe k <<< Map.lookup k

lookupArray :: forall k v. Ord k => Array k -> Map k v -> Array (k /\ v)
lookupArray keys dict = keys `zip'` vals
  where
  vals = filterMap (_ `Map.lookup` dict) keys

