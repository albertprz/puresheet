module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Components.Table.Models (AppState)
import App.Interpreters.Builtins as Builtins
import App.SyntaxTrees.Common (Var(..), VarOp)
import App.SyntaxTrees.FnDef (Arity(..), Associativity(..), BuiltinFnInfo, FnBody(..), FnDef(..), FnInfo, FnVar(..), Object(..), OpInfo)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Enum (fromEnum, toEnum)
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

evalExprInApp
  :: forall m. Partial => MonadState AppState m => FnBody -> m Object
evalExprInApp expr = do
  appState <- get
  pure $ evalState (evalExpr expr)
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    , builtinFnsMap: Builtins.builtinFnsMap
    , localFnsMap: Map.empty
    }

evalExpr
  :: forall m. Partial => MonadState LocalFormulaCtx m => FnBody -> m Object

evalExpr (FnApply fnExpr args) = do
  fnObj <- evalExpr fnExpr
  argObjs <- traverse evalExpr args
  case fnObj of
    FnObj fnInfo -> evalFn fnInfo argObjs
    BuiltinFnObj fnInfo -> evalBuiltinFn fnInfo argObjs
    _ -> unsafeCrashWith (show fnObj <> " is not a function")

evalExpr (InfixFnApply fnOps args) =
  do
    { operatorsMap } <- get
    evalExpr =<< nestInfixFns (lookupArray fnOps operatorsMap) args

evalExpr (LeftOpSection fnOp body) =
  pure $ FnObj
    { body: (InfixFnApply [ fnOp ] [ body, varFn argId ])
    , params: [ Var argId ]
    }

evalExpr (RightOpSection body fnOp) =
  pure $ FnObj
    { body: (InfixFnApply [ fnOp ] [ varFn argId, body ])
    , params: [ Var argId ]
    }

evalExpr (WhereExpr fnBody bindings) =
  traverse_ registerLocalFn bindings *> evalExpr fnBody

evalExpr (ListRange x y) = evalExpr $ FnApply (varFn "range") [ x, y ]

evalExpr (List list) =
  evalExpr $ foldl (FnApply (varFn "snoc") <.. arr2)
    (FnApply (varFn "emptyList") [])
    list

evalExpr (FnVar' (Var' fn)) = do
  fnInfo <- lookupFn fn (_.builtinFnsMap)
  case fnInfo of
    Just x -> pure $ BuiltinFnObj x
    Nothing -> FnObj <$> unsafeLookupFn fn
      (\x -> Map.union x.localFnsMap x.fnsMap)

evalExpr (FnOp fnOp) = do
  { fnName } <- unsafeLookupFn fnOp (_.operatorsMap)
  fnInfo <- unsafeLookupFn fnName (_.fnsMap)
  pure $ FnObj fnInfo

evalExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ maybe (ListObj []) cellValueToObj $ Map.lookup cell
      tableData

evalExpr (CellValue' cellValue) = pure $ cellValueToObj cellValue

evalFn
  :: forall m
   . Partial
  => MonadState LocalFormulaCtx m
  => FnInfo
  -> Array Object
  -> m Object
evalFn { body, params } args = do
  st <- get
  let
    evalInContext expr f =
      evalState (evalExpr expr)
        ( st
            { builtinFnsMap = Map.union argBindings st.builtinFnsMap
            , localFnsMap = f $ st.localFnsMap
            }
        )
  if unappliedArgsNum == 0 then
    pure $ evalInContext body identity
  else if unappliedArgsNum > 0 then
    pure $ evalInContext (varFn argId)
      ( Map.insert (Var argId)
          { body, params: takeEnd' unappliedArgsNum params }
      )
  else unsafeCrashWith "Too many arguments supplied to current function"
  where
  unappliedArgsNum = length params - length args
  argBindings = Map.fromFoldable
    $ rmap (\arg -> { fn: const arg, arity: A0 })
    <$> zip' params args

evalBuiltinFn
  :: forall m
   . Partial
  => MonadState LocalFormulaCtx m
  => BuiltinFnInfo
  -> Array Object
  -> m Object
evalBuiltinFn { fn, arity } args =
  if unappliedArgsNum == 0 then
    pure $ fn args
  else if unappliedArgsNum > 0 then
    pure $ BuiltinFnObj
      { fn: \newArgs -> fn (args <> newArgs)
      , arity: unsafeFromJust $ toEnum unappliedArgsNum
      }
  else
    unsafeCrashWith "Too many arguments supplied to current function"
  where
  unappliedArgsNum = fromEnum arity - length args

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

registerLocalFn :: forall m. MonadState LocalFormulaCtx m => FnDef -> m Unit
registerLocalFn (FnDef fnName params body) =
  modify_ \st ->
    st { localFnsMap = Map.insert fnName { params, body } st.localFnsMap }

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

varFn :: String -> FnBody
varFn = FnVar' <<< Var' <<< Var

argId :: String
argId = "__arg__"
