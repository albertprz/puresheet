module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Components.Table.Models (AppState)
import App.SyntaxTrees.Common (Var, VarOp)
import App.SyntaxTrees.FnDef (Associativity(..), FnBody(..), FnInfo, FnVar(..), Object(..), OpInfo)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Filterable (filterMap)
import Data.Map as Map
import Partial.Unsafe (unsafeCrashWith)

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map Var FnInfo
  , localFnsMap :: Map Var FnInfo
  , operatorsMap :: Map VarOp OpInfo
  , builtinFnsMap :: Map Var ((Array Object -> Object) /\ Int)
  }

evalFormula :: AppState -> FnBody -> Object
evalFormula = (fromMaybe (ListObj []) <<< extractObject) <.. evalExpr

evalExpr :: AppState -> FnBody -> FnBody
evalExpr appState expr = evalState (reduceExpr expr) localFormulaCtx

  where
  localFormulaCtx =
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , builtinFnsMap: appState.formulaCtx.builtinFnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    , localFnsMap: Map.empty
    }

reduceExpr :: forall m. MonadState LocalFormulaCtx m => FnBody -> m FnBody
reduceExpr (FnApply (FnVar' (Var' fnName)) args)
  | Just args' <- traverse extractObject args =
      do
        (fn /\ arity) <- lookupFn fnName (_.builtinFnsMap)
        if length args' == arity then
          pure $ Object' $ fn args'
        else unsafeCrashWith "Partially applied Fn"

  | otherwise =
      do
        { body, params } <- lookupFn fnName
          (\x -> Map.union x.localFnsMap x.fnsMap)
        if (length args :: Int) == length params then
          reduceExpr $ FnApply body args
        else unsafeCrashWith "Partially applied Fn"

reduceExpr (FnApply fn args) =
  reduceExpr =<< (FnApply <$> reduceExpr fn <*> traverse reduceExpr args)

reduceExpr (InfixFnApply fns args) =
  do
    { operatorsMap } <- get
    reduceExpr =<< flattenInfixFns (lookupArray fns operatorsMap) args

reduceExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ Object' $ maybe (ListObj []) cellValueToObj $ Map.lookup cell
      tableData

reduceExpr x = pure x

flattenInfixFns
  :: forall m
   . MonadState LocalFormulaCtx m
  => Array (VarOp /\ OpInfo)
  -> Array FnBody
  -> m FnBody
flattenInfixFns [ (_ /\ { fnName }) ] args = do
  pure $ FnApply (FnVar' $ Var' fnName) args

flattenInfixFns fnOps args = flattenInfixFns newFns newArgs
  where
  (fnOp /\ { fnName, associativity }) = unsafeFromJust $ maximumBy
    (compare `on` (_.precedence <<< snd))
    fnOps
  (indexFn /\ sliceFn) =
    case associativity of
      L -> (findIndex' /\ sliceNext')
      R -> (findLastIndex' /\ slicePrev')
  idx = unsafeFromJust $ indexFn ((_ == fnOp) <<< fst) fnOps
  newFns = fold $ deleteAt' idx fnOps
  redexArgs = sliceFn 2 idx args
  newArgs = fold $ deleteAt' (idx + 1) $ fold
    $ updateAt' idx (FnApply (FnVar' $ Var' fnName) redexArgs) args

cellValueToObj :: CellValue -> Object
cellValueToObj = case _ of
  (BoolVal x) -> BoolObj x
  (IntVal x) -> IntObj x
  (FloatVal x) -> FloatObj x
  (CharVal x) -> CharObj x
  (StringVal x) -> StringObj x

lookupFn
  :: forall k v s m
   . MonadState s m
  => Show k
  => Ord k
  => k
  -> (s -> Map k v)
  -> m v
lookupFn fnName fetchFnMap =
  fromMaybe (unsafeCrashWith $ "Unknown function: " <> show fnName)
    <$> Map.lookup fnName
    <$> gets fetchFnMap

lookupArray :: forall k v. Ord k => Array k -> Map k v -> Array (k /\ v)
lookupArray keys dict = keys `zip'` vals
  where
  vals = filterMap (_ `Map.lookup` dict) keys

extractObject :: FnBody -> Maybe Object
extractObject (Object' x) = Just x
extractObject (CellValue' x) = Just $ cellValueToObj x
extractObject _ = Nothing
