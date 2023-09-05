module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Components.Table.Models (AppState)
import App.Interpreters.Builtins as Builtins
import App.SyntaxTrees.Common (Var(..), VarOp)
import App.SyntaxTrees.FnDef (Associativity(..), BuiltinFnInfo, FnBody(..), FnInfo, FnVar(..), Object(..), OpInfo)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Enum (fromEnum)
import Data.Filterable (filterMap)
import Data.Map as Map
import Partial.Unsafe (unsafeCrashWith)

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map Var FnInfo
  , localFnsMap :: Map Var FnInfo
  , operatorsMap :: Map VarOp OpInfo
  , builtinFnsMap :: Map Var BuiltinFnInfo
  }

evalFormula :: AppState -> FnBody -> Object
evalFormula = (fromMaybe (ListObj []) <<< extractObject) <.. evalExpr

evalExpr :: AppState -> FnBody -> FnBody
evalExpr appState expr = evalState (reduceExpr expr) localFormulaCtx
  where
  localFormulaCtx =
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    , builtinFnsMap: Builtins.builtinFnsMap
    , localFnsMap: Map.empty
    }

reduceExpr :: forall m. MonadState LocalFormulaCtx m => FnBody -> m FnBody
reduceExpr (FnApply (Object' (BuiltinFnObj { fn, arity })) args)
  | Just args' <- traverse extractObject args =
      if length args' == fromEnum arity then
        pure $ Object' $ fn args'
      else unsafeCrashWith "Partially applied Fn"

reduceExpr (FnApply (Object' (FnObj { body, params })) args) =
  if (length args :: Int) == length params then
    reduceExpr $ FnApply body args
  else unsafeCrashWith "Partially applied Fn"

reduceExpr (FnApply fn args) =
  reduceExpr =<< (FnApply <$> reduceExpr fn <*> traverse reduceExpr args)

reduceExpr (InfixFnApply fnOps args) =
  do
    { operatorsMap } <- get
    reduceExpr =<< nestInfixFns (lookupArray fnOps operatorsMap) args

reduceExpr (ListRange x y) = reduceExpr $ FnApply (varFn "range") [ x, y ]

reduceExpr (List list) =
  reduceExpr $ foldr (FnApply (varFn "cons") <.. arr2)
    (Object' $ ListObj [])
    list

reduceExpr (FnVar' (Var' fn)) = do
  fnInfo <- lookupFn fn (_.builtinFnsMap)
  case fnInfo of
    Just x -> pure $ Object' $ BuiltinFnObj x
    Nothing -> Object' <<< FnObj <$> unsafeLookupFn fn
      (\x -> Map.union x.localFnsMap x.fnsMap)

reduceExpr (FnOp fnOp) = do
  { fnName } <- unsafeLookupFn fnOp (_.operatorsMap)
  fnInfo <- unsafeLookupFn fnName (_.fnsMap)
  reduceExpr $ Object' $ FnObj fnInfo

reduceExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ Object' $ maybe (ListObj []) cellValueToObj $ Map.lookup cell
      tableData

reduceExpr (CellValue' cellValue) = pure $ Object' $ cellValueToObj cellValue

reduceExpr (Object' obj) = pure $ Object' obj

reduceExpr x = pure x

nestInfixFns
  :: forall m
   . MonadState LocalFormulaCtx m
  => Array (VarOp /\ OpInfo)
  -> Array FnBody
  -> m FnBody
nestInfixFns [ (_ /\ { fnName }) ] args = do
  pure $ FnApply (FnVar' $ Var' fnName) args

nestInfixFns fnOps args = nestInfixFns newFns newArgs
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

unsafeLookupFn
  :: forall k v s m
   . MonadState s m
  => Show k
  => Ord k
  => k
  -> (s -> Map k v)
  -> m v
unsafeLookupFn fnName =
  map (fromMaybe (unsafeCrashWith $ "Unknown function: " <> show fnName))
    <<< lookupFn fnName

lookupFn
  :: forall k v s m
   . MonadState s m
  => Show k
  => Ord k
  => k
  -> (s -> Map k v)
  -> m (Maybe v)
lookupFn fnName fetchFnMap = Map.lookup fnName <$> gets fetchFnMap

lookupArray :: forall k v. Ord k => Array k -> Map k v -> Array (k /\ v)
lookupArray keys dict = keys `zip'` vals
  where
  vals = filterMap (_ `Map.lookup` dict) keys

extractObject :: FnBody -> Maybe Object
extractObject (Object' x) = Just x
extractObject (CellValue' x) = Just $ cellValueToObj x
extractObject _ = Nothing

varFn :: String -> FnBody
varFn = FnVar' <<< Var' <<< Var

