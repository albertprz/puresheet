module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..), buildCell)
import App.Components.Table.Models (AppState)
import App.Interpreters.Builtins as Builtins
import App.Interpreters.Object (cellValueToObj, extractBool, extractNList, objectToCellValues)
import App.SyntaxTrees.Common (Var(..), VarOp)
import App.SyntaxTrees.FnDef (Associativity(..), BuiltinFnInfo, CaseBinding(..), FnBody(..), FnDef(..), FnInfo, FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), Object(..), OpInfo, PatternGuard(..))
import App.SyntaxTrees.Pattern (Pattern(..))
import App.Utils.Common (spyShow)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Map as Map
import Data.Set as Set
import Matrix as Matrix
import Partial.Unsafe (unsafeCrashWith)

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map Var FnInfo
  , operatorsMap :: Map VarOp OpInfo
  }

evalFormula
  :: forall m
   . MonadState AppState m
  => Cell
  -> FnBody
  -> m (Maybe (Map Cell CellValue))
evalFormula { column, row } body = do
  { columns, rows } <- get
  obj <- evalExprInApp (spyShow body)
  let
    toCellMap cellMatrix =
      Map.filter nonEmptyCellValue
        $ Map.fromFoldable
        $ filterMap toCellPair
        $ Matrix.toIndexedArray cellMatrix
    toCellPair { x, y, value } =
      (_ /\ value) <<< uncurry buildCell <$>
        bisequence
          ( getElemSat (_ + x) columns column /\
              getElemSat (_ + y) rows row
          )
  pure $ toCellMap <$> join (partialMaybe objectToCellValues $ obj)

evalExprInApp
  :: forall m. MonadState AppState m => FnBody -> m Object
evalExprInApp expr = do
  appState <- get
  pure $ evalState (evalExpr expr)
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    }

evalExpr
  :: forall m. MonadState LocalFormulaCtx m => FnBody -> m Object

evalExpr (FnApply fnExpr args) = do
  fnObj <- evalExpr fnExpr
  argObjs <- traverse evalExpr args
  case fnObj of
    FnObj fnInfo -> evalFn fnInfo args
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

evalExpr (CondExpr conds) = do
  st <- get
  pure $ fromMaybe (unsafeCrashWith "Unreachable pattern match") $
    findMap (evalGuardedFnBody st) conds

evalExpr (SwitchExpr matchee cases) = do
  st <- get
  result <- evalExpr matchee
  pure $ fromMaybe (unsafeCrashWith "Unreachable pattern match") $
    findMap (evalCaseBinding st result) cases

evalExpr
  ( ListRange (Cell' { column: colX, row: rowX })
      (Cell' { column: colY, row: rowY })
  ) | rowX == rowY =
  evalExpr $ List $ toArray
    $ (\column -> Cell' { column, row: rowX })
    <$>
      (colX .. colY)

evalExpr
  ( ListRange (Cell' { column: colX, row: rowX })
      (Cell' { column: colY, row: rowY })
  ) | colX == colY =
  evalExpr $ List $ (\row -> Cell' { row, column: colX }) <$>
    toArray (rowX .. rowY)

evalExpr (ListRange x y) = evalExpr $ FnApply (varFn "range") [ x, y ]

evalExpr
  ( MatrixRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  ) =
  evalExpr $ List (List <$> matrix)
  where
  matrix = do
    row <- toArray $ rowX .. rowY
    pure $ do
      column <- toArray $ colX .. colY
      pure $ Cell' { column, row }

evalExpr (List list) =
  evalExpr
    $ foldl (FnApply (varFn "snoc") <.. arr2)
        (Object' $ ListObj [])
        list

evalExpr (FnVar' (Var' fn))
  | Just fnInfo <- Map.lookup fn Builtins.builtinFnsMap = pure $ BuiltinFnObj
      fnInfo
  | otherwise = FnObj <$> unsafeLookupFn fn _.fnsMap

evalExpr (FnOp fnOp) = do
  { fnName } <- unsafeLookupFn fnOp _.operatorsMap
  fnInfo <- unsafeLookupFn fnName _.fnsMap
  pure $ FnObj fnInfo

evalExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ maybe NullObj cellValueToObj $ Map.lookup cell
      tableData

evalExpr (CellValue' cellValue) = pure $ cellValueToObj cellValue

evalExpr (Object' obj) = pure obj

evalFn
  :: forall m
   . MonadState LocalFormulaCtx m
  => FnInfo
  -> Array FnBody
  -> m Object
evalFn { body, params } args = do
  st <- get
  if unappliedArgsNum == 0 then
    evalInContext st body
  else if unappliedArgsNum > 0 then
    evalInContext st
      (Object' $ FnObj { body, params: takeEnd' unappliedArgsNum params })
  else unsafeCrashWith "Too many arguments supplied to current function"
  where
  unappliedArgsNum = length params - length args
  evalInContext st expr =
    pure $ evalState (evalExpr expr)
      (st { fnsMap = Map.union argBindings st.fnsMap })
  argBindings = Map.fromFoldable
    $ rmap (\arg -> { body: arg, params: [] })
    <$> zip' params args

evalBuiltinFn
  :: forall m
   . MonadState LocalFormulaCtx m
  => BuiltinFnInfo
  -> Array Object
  -> m Object
evalBuiltinFn { fn, arity, defaultParams } args =
  if unappliedArgsNum == 0 then
    pure $ fromMaybe' (\_ -> fn args) defaultResult
  else if unappliedArgsNum > 0 then
    pure $ BuiltinFnObj
      { fn: \newArgs -> fn (args <> newArgs)
      , arity: unsafeFromJust $ toEnum unappliedArgsNum
      , defaultParams: Set.filter zeroOrPos
          $ Set.map (_ - length args) defaultParams
      }
  else
    unsafeCrashWith "Too many arguments supplied to current function"
  where
  unappliedArgsNum = fromEnum arity - length args
  defaultResult =
    if NullObj `elem` args && not (null defaultParams) then
      find
        (_ /= NullObj)
        (filterByIndexes defaultParams args)
    else
      Nothing

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
  indexFn =
    case associativity of
      L -> findIndex'
      R -> findLastIndex'
  idx = unsafeFromJust $ indexFn ((_ == fnOp) <<< fst) fnOps
  newFns = fold $ deleteAt' idx fnOps
  redexArgs = sliceNext' 2 idx args
  newArgs = fold $ deleteAt' (idx + 1) $ fold
    $ updateAt' idx (FnApply (FnVar' $ Var' fnName) redexArgs) args

evalCaseBinding
  :: LocalFormulaCtx
  -> Object
  -> CaseBinding
  -> Maybe Object
evalCaseBinding ctx matchee (CaseBinding pattern body) =
  evalState action ctx
  where
  action = ifM (evalPatternBinding pattern matchee)
    (Just <$> evalMaybeGuardedFnBody body)
    (pure Nothing)

evalMaybeGuardedFnBody
  :: forall m
   . MonadState LocalFormulaCtx m
  => MaybeGuardedFnBody
  -> m Object
evalMaybeGuardedFnBody (Guarded conds) =
  evalExpr $ CondExpr conds

evalMaybeGuardedFnBody (Standard body) =
  evalExpr body

evalGuardedFnBody
  :: LocalFormulaCtx
  -> GuardedFnBody
  -> Maybe Object
evalGuardedFnBody ctx (GuardedFnBody guard body) =
  evalState action ctx
  where
  action = ifM (evalGuard guard)
    (Just <$> evalExpr body)
    (pure Nothing)

evalGuard
  :: forall m
   . MonadState LocalFormulaCtx m
  => Guard
  -> m Boolean
evalGuard (Guard guardPatterns) =
  and <$> traverse evalPatternGuard guardPatterns

evalGuard Otherwise = pure true

evalPatternGuard
  :: forall m
   . MonadState LocalFormulaCtx m
  => PatternGuard
  -> m Boolean
evalPatternGuard (PatternGuard pattern body) =
  evalPatternBinding pattern =<< evalExpr body

evalPatternGuard (SimpleGuard body) =
  extractBool <$> evalExpr body

evalPatternBinding
  :: forall m
   . MonadState LocalFormulaCtx m
  => Pattern
  -> Object
  -> m Boolean
evalPatternBinding (VarPattern var) result =
  registerLocalFn (FnDef var [] $ Object' result) $> true

evalPatternBinding (LitPattern cellValue) result =
  pure $ cellValueToObj cellValue == result

evalPatternBinding (AliasedPattern var pattern) result =
  registerLocalFn (FnDef var [] $ Object' result) *>
    evalPatternBinding pattern result

evalPatternBinding (ListPattern patterns) result
  | Just idx <- findIndex' (_ == Spread) patterns
  , length (filter (_ == Spread) patterns) == 1
  , ListObj results <- result =
      evalPatternBinding
        (ListPattern $ patternsBegin <> patternsEnd)
        (ListObj $ resultsBegin <> resultsEnd)
      where
      { before, after } = splitAt' idx patterns
      (patternsBegin /\ patternsEnd) = (before /\ fold (tail' after))
      resultsBegin = take' (length patternsBegin) results
      resultsEnd = takeEnd' (length patternsEnd) results

evalPatternBinding (ListPattern patterns) result
  | Just results <- extractNList (length patterns) result =
      and <$> traverse (uncurry evalPatternBinding)
        (patterns `zip'` results)

evalPatternBinding (ListPattern _) _ =
  pure false

evalPatternBinding (Wildcard) _ =
  pure true

evalPatternBinding (Spread) _ =
  pure true

registerLocalFn :: forall m. MonadState LocalFormulaCtx m => FnDef -> m Unit
registerLocalFn (FnDef fnName params body) =
  modify_ \st ->
    st { fnsMap = Map.insert fnName { params, body } st.fnsMap }

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

nonEmptyCellValue :: CellValue -> Boolean
nonEmptyCellValue (StringVal "") = false
nonEmptyCellValue _ = true

filterByIndexes :: forall a f. Foldable f => f Int -> Array a -> Array a
filterByIndexes idxs arr = fst <$>
  filter (\(_ /\ idx) -> idx `elem` idxs)
    (arr `zip'` toArray (0 .. (length arr - 1)))

varFn :: String -> FnBody
varFn = FnVar' <<< Var' <<< Var

argId :: String
argId = "__arg__"

