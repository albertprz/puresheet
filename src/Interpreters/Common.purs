module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..), buildCell)
import App.Components.Table.Models (AppState)
import App.Interpreters.Builtins as Builtins
import App.Interpreters.Object (cellValueToObj, extractBool, extractNList, objectToCellValues)
import App.SyntaxTrees.Common (Var(..), VarOp)
import App.SyntaxTrees.FnDef (Arity(..), Associativity(..), BuiltinFnInfo, CaseBinding(..), FnBody(..), FnDef(..), FnInfo, FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), Object(..), OpInfo, PatternGuard(..))
import App.SyntaxTrees.Pattern (Pattern(..))
import App.Utils.Common (spyShow)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Monad.Except (ExceptT, except, runExceptT)
import Data.Map as Map
import Data.Set as Set
import Matrix as Matrix

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map Var FnInfo
  , operatorsMap :: Map VarOp OpInfo
  }

type EvalM a = forall m. MonadState LocalFormulaCtx m => ExceptT Error m a

evalFormula
  :: forall m
   . MonadState AppState m
  => Cell
  -> FnBody
  -> ExceptT Error m (Map Cell CellValue)
evalFormula { column, row } body = do
  { columns, rows } <- get
  obj <- evalExprInApp body
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
  except $ note emptyError $ toCellMap <$> join
    (partialMaybe objectToCellValues $ obj)

evalExprInApp
  :: forall m. MonadState AppState m => FnBody -> ExceptT Error m Object
evalExprInApp expr = do
  appState <- get
  except $ evalState (runExceptT $ evalExpr expr)
    { tableData: appState.tableData
    , fnsMap: appState.formulaCtx.fnsMap
    , operatorsMap: appState.formulaCtx.operatorsMap
    }

evalExpr :: FnBody -> EvalM Object

evalExpr (FnApply fnExpr args) = do
  fnObj <- evalExpr fnExpr
  argObjs <- traverse (\x -> evalExpr x) args
  case (spyShow fnObj) of
    FnObj fnInfo -> evalFn fnInfo args
    BuiltinFnObj fnInfo -> evalBuiltinFn fnInfo argObjs
    _ -> raiseError ("Value '" <> show fnObj <> "' is not a function")

evalExpr (InfixFnApply fnOps args) =
  do
    { operatorsMap } <- get
    (\x -> evalExpr x) =<< nestInfixFns (lookupArray fnOps operatorsMap) args

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
  traverse_ (\x -> registerLocalFn x) bindings *> evalExpr fnBody

evalExpr (CondExpr conds) = do
  st <- get
  except $ note (error "Unreachable pattern match") $
    findMap (evalGuardedFnBody st) conds

evalExpr (SwitchExpr matchee cases) = do
  st <- get
  result <- evalExpr matchee
  except $ note (error "Unreachable pattern match") $
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
  | Just fnInfo <- Map.lookup fn Builtins.builtinFnsMap =
      evalExpr $ Object' $ BuiltinFnObj
        fnInfo
  | otherwise = (\x -> evalExpr <<< Object' <<< FnObj $ x) =<< lookupFn fn
      _.fnsMap

evalExpr (FnOp fnOp) = do
  { fnName } <- lookupFn fnOp _.operatorsMap
  fnInfo <- lookupFn fnName _.fnsMap
  pure $ FnObj fnInfo

evalExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ maybe NullObj cellValueToObj $ Map.lookup cell
      tableData

evalExpr (CellValue' cellValue) = pure $ cellValueToObj cellValue

evalExpr (Object' (FnObj { body, params: [] })) = evalExpr body

evalExpr (Object' (BuiltinFnObj { fn, arity: A0 })) = pure $ fn []

evalExpr (Object' obj) = pure obj

evalFn
  :: FnInfo
  -> Array FnBody
  -> EvalM Object
evalFn { body, params } args = do
  st <- get
  if unappliedArgsNum == 0 then
    evalInContext st body
  else if unappliedArgsNum > 0 then
    evalInContext st
      (Object' $ FnObj { body, params: takeEnd' unappliedArgsNum params })
  else raiseError "TypeError: Too many arguments supplied to current function"
  where
  unappliedArgsNum = length params - length args
  evalInContext st expr = except $
    evalState (runExceptT $ evalExpr expr)
      (st { fnsMap = Map.union argBindings st.fnsMap })
  argBindings = Map.fromFoldable
    $ rmap (\arg -> { body: arg, params: [] })
    <$> zip' params args

evalBuiltinFn
  :: BuiltinFnInfo
  -> Array Object
  -> EvalM Object
evalBuiltinFn { fn, arity, defaultParams } args =
  if unappliedArgsNum == 0 then
    except
      $ note
          ( error
              ( "TypeError: The combination of argument types is not allowed: "
                  <> show args
              )
          )
      $ fromMaybe' (\_ -> partialMaybe fn args) (pure <$> defaultResult)
  else if unappliedArgsNum > 0 then
    pure $ BuiltinFnObj
      { fn: \newArgs -> fn (args <> newArgs)
      , arity: unsafeFromJust $ toEnum unappliedArgsNum
      , defaultParams: Set.filter zeroOrPos
          $ Set.map (_ - length args) defaultParams
      }
  else
    raiseError "TypeError: Too many arguments supplied to current function"
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
  :: Array (VarOp /\ OpInfo)
  -> Array FnBody
  -> EvalM FnBody
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
  hush $ evalState action ctx
  where
  action = runExceptT $ ifM (evalPatternBinding pattern matchee)
    (evalMaybeGuardedFnBody body)
    raiseEmptyError

evalMaybeGuardedFnBody
  :: MaybeGuardedFnBody
  -> EvalM Object
evalMaybeGuardedFnBody (Guarded conds) =
  evalExpr $ CondExpr conds

evalMaybeGuardedFnBody (Standard body) =
  evalExpr body

evalGuardedFnBody
  :: LocalFormulaCtx
  -> GuardedFnBody
  -> Maybe Object
evalGuardedFnBody ctx (GuardedFnBody guard body) =
  hush $ evalState action ctx
  where
  action = runExceptT $ ifM (evalGuard guard)
    (evalExpr body)
    raiseEmptyError

evalGuard :: Guard -> EvalM Boolean
evalGuard (Guard guardPatterns) = do
  and <$> traverse (\x -> evalPatternGuard x) guardPatterns

evalGuard Otherwise = pure true

evalPatternGuard :: PatternGuard -> EvalM Boolean
evalPatternGuard (PatternGuard pattern body) = do
  (\x -> evalPatternBinding pattern x) =<< evalExpr body

evalPatternGuard (SimpleGuard body) =
  except <<< extractBool =<< evalExpr body

evalPatternBinding
  :: Pattern -> Object -> EvalM Boolean
evalPatternBinding (VarPattern var) result =
  registerLocalFn (FnDef var [] $ Object' result) $> true

evalPatternBinding (LitPattern cellValue) result =
  pure $ spyShow $ cellValueToObj cellValue == result

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
      and <$> traverse (\(x /\ y) -> evalPatternBinding x y)
        (patterns `zip'` results)

evalPatternBinding (ListPattern _) _ =
  lift $ pure false

evalPatternBinding (Wildcard) _ =
  pure true

evalPatternBinding (Spread) _ =
  pure true

registerLocalFn :: FnDef -> EvalM Unit
registerLocalFn (FnDef fnName params body) =
  modify_ \st ->
    st { fnsMap = Map.insert fnName { params, body } st.fnsMap }

lookupFn
  :: forall k v
   . Show k
  => Ord k
  => k
  -> (LocalFormulaCtx -> Map k v)
  -> EvalM v
lookupFn fnName fetchFnMap = do
  fnMap <- gets fetchFnMap
  except
    $ note (error $ "Unknown function: '" <> show fnName <> "'")
    $ Map.lookup fnName fnMap

raiseEmptyError :: forall a. EvalM a
raiseEmptyError = except <<< Left $ emptyError

raiseError :: forall a. String -> EvalM a
raiseError x = except <<< Left <<< error $ x

emptyError :: Error
emptyError = error ""

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
