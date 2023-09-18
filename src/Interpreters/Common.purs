module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..), buildCell)
import App.Components.Table.Models (AppState)
import App.Interpreters.Builtins as Builtins
import App.Interpreters.Object (cellValueToObj, extractBool, extractNList, objectToCellValues)
import App.SyntaxTrees.Common (Var(..), VarOp)
import App.SyntaxTrees.FnDef (Arity(..), Associativity(..), BuiltinFnInfo, CaseBinding(..), FnBody(..), FnDef(..), FnInfo, FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), Object(..), OpInfo, PatternGuard(..), Scope(..))
import App.SyntaxTrees.Pattern (Pattern(..))
import Bookhound.FatPrelude (findJust, hasSome)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Monad.Except (ExceptT, except, runExceptT)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Tree (Forest, Tree, mkTree)
import Data.Tree.Zipper (Loc, children, findFromRoot, fromTree, insertAfter, insertChild, node, parents, siblings, toTree, up, value)
import Matrix as Matrix

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map (Scope /\ Var) FnInfo
  , operatorsMap :: Map VarOp OpInfo
  , argsMap :: Map (Scope /\ Var) FnInfo
  , scope :: Scope
  , scopeLoc :: Loc Scope
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
    , argsMap: Map.empty
    , scope: zero
    , scopeLoc: fromTree $ mkLeaf zero
    }

evalExpr :: FnBody -> EvalM Object

evalExpr (FnApply fnExpr args) = do
  fnObj <- evalExpr fnExpr
  argObjs <- traverse (\x -> evalExpr x) args
  case fnObj of
    FnObj fnInfo -> evalFn fnInfo (Object' <$> argObjs)
    BuiltinFnObj fnInfo -> evalBuiltinFn fnInfo argObjs
    _ -> raiseError
      ("Type Error: Value '" <> show fnObj <> "' is not a function")

evalExpr (InfixFnApply fnOps args) =
  do
    { operatorsMap } <- get
    (\x -> evalExpr x) =<< nestInfixFns (lookupArray fnOps operatorsMap) args

evalExpr (LeftOpSection fnOp body) = do
  { scope } <- get
  pure $ FnObj
    { body: (InfixFnApply [ fnOp ] [ body, varFn argId ])
    , params: [ Var argId ]
    , scope
    }

evalExpr (RightOpSection body fnOp) = do
  { scope } <- get
  pure $ FnObj
    { body: (InfixFnApply [ fnOp ] [ varFn argId, body ])
    , params: [ Var argId ]
    , scope
    }

evalExpr (WhereExpr fnBody bindings) =
  registerBindings bindings *> evalExpr fnBody

evalExpr (CondExpr conds) = do
  st <- get
  except $ note (error "Match Error: Unreachable pattern match") $
    findMap (evalGuardedFnBody st) conds

evalExpr (SwitchExpr matchee cases) = do
  st <- get
  result <- evalExpr matchee
  except $ note (error "Match Error: Unreachable pattern match") $
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
      pure $ BuiltinFnObj fnInfo
  | otherwise =
      (\x -> evalExpr <<< Object' <<< FnObj $ x) =<< lookupFn fn

evalExpr (FnOp fnOp) = do
  { fnName } <- lookupOperator fnOp
  evalExpr (FnVar' $ Var' fnName)

evalExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ maybe NullObj cellValueToObj $ Map.lookup cell
      tableData

evalExpr (CellValue' cellValue) = pure $ cellValueToObj cellValue

evalExpr (Object' (FnObj fnInfo@{ params: [] })) =
  evalFn fnInfo []

evalExpr (Object' (BuiltinFnObj fnInfo@{ arity: A0 })) =
  evalBuiltinFn fnInfo []

evalExpr (Object' obj) = pure obj

evalFn
  :: FnInfo
  -> Array FnBody
  -> EvalM Object
evalFn { body, params, scope } args = do
  ctx <- get
  if unappliedArgsNum == 0 then do
    except $ evalState (runExceptT $ evalExpr body)
      ( ctx
          { scope = scope
          , argsMap = Map.union argBindings ctx.argsMap
          , scopeLoc = goToNode scope ctx.scopeLoc
          }
      )
  else if unappliedArgsNum > 0 then
    evalExpr
      ( Object' $ FnObj
          { body, params: takeEnd' unappliedArgsNum params, scope }
      )
  else raiseError "TypeError: Too many arguments supplied to current function"
  where
  unappliedArgsNum = length params - length args
  argBindings = Map.fromFoldable
    $ rmap (\arg -> { body: arg, params: [], scope: scope })
    <$> zip' ((\x -> (scope /\ x)) <$> params) args

evalBuiltinFn :: BuiltinFnInfo -> Array Object -> EvalM Object
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
    raiseError ("TypeError: Too many arguments supplied to current function")
  where
  unappliedArgsNum = fromEnum arity - length args
  defaultResult =
    if NullObj `elem` args && hasSome defaultParams then
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
evalPatternGuard (PatternGuard pattern body) =
  (\x -> evalPatternBinding pattern x) =<< evalExpr body

evalPatternGuard (SimpleGuard body) =
  except <<< extractBool =<< evalExpr body

evalPatternBinding
  :: Pattern -> Object -> EvalM Boolean
evalPatternBinding (VarPattern var) result = do
  { scope } <- get
  registerLocalFn scope (FnDef var [] $ Object' result) $> true

evalPatternBinding (LitPattern cellValue) result =
  pure $ cellValueToObj cellValue == result

evalPatternBinding (AliasedPattern var pattern) result = do
  { scope } <- get
  registerLocalFn scope (FnDef var [] $ Object' result) *>
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

evalPatternBinding Wildcard _ =
  pure true

evalPatternBinding Spread _ =
  pure true

registerBindings :: Array FnDef -> EvalM Unit
registerBindings bindings = do
  { scope, scopeLoc } <- get
  let (Scope maxScope) = fromMaybe scope $ maximum $ toTree scopeLoc
  let scopes = Scope <<< (_ + maxScope) <$> toArray (1 .. length bindings)
  traverse_ (\(n /\ x) -> registerLocalFn n x) (scopes `zip'` bindings)
  modify_ \st -> st
    { scopeLoc = appendChildren (mkLeaf <$> List.fromFoldable scopes)
        st.scopeLoc
    }

registerLocalFn :: Scope -> FnDef -> EvalM Unit
registerLocalFn newScope (FnDef fnName params body) =
  modify_ \st ->
    st
      { fnsMap = Map.insert (newScope /\ fnName)
          { params, body, scope: newScope }
          st.fnsMap
      }

-- Scope resolution:
-- 1. Children bindings
-- 2. Fn args
-- 3. Siblings bindings
-- 4. Free variables (Closed over values)
lookupFn :: Var -> EvalM FnInfo
lookupFn fnName = do
  { fnsMap, argsMap, scope: scope, scopeLoc } <- get
  let
    lookupVar n = Map.lookup (n /\ fnName) fnsMap
    lookupArg n = Map.lookup (n /\ fnName) argsMap
    childrenLookup = lookupVar <$> childrenValues scope scopeLoc
    siblingsLookup = lookupVar <$> siblingsValues scope scopeLoc
    argsLookup = lookupArg <$> nodeValues scope scopeLoc
    freeVarsLookup = lookupArg <$> ancestorsValues scope scopeLoc
  except
    $ note (error $ "Lexical Error: Unknown value: '" <> show fnName <> "'")
    $ findJust
        (childrenLookup <> argsLookup <> siblingsLookup <> freeVarsLookup)

lookupOperator :: VarOp -> EvalM OpInfo
lookupOperator opName = do
  { operatorsMap } <- get
  except
    $ note (error $ "Lexical Error: Unknown operator: '" <> show opName <> "'")
    $ Map.lookup opName operatorsMap

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

nodeValues :: forall a. Eq a => a -> Loc a -> List a
nodeValues = findValues (pure <<< mkLeaf <<< value)

childrenValues :: forall a. Eq a => a -> Loc a -> List a
childrenValues = findValues children

siblingsValues :: forall a. Eq a => a -> Loc a -> List a
siblingsValues = findValues siblings

ancestorsValues :: forall a. Eq a => a -> Loc a -> List a
ancestorsValues val loc =
  findValues (map node <<< parents) val loc

findValues :: forall a. Eq a => (Loc a -> Forest a) -> a -> Loc a -> List a
findValues findFn val loc = value <<< fromTree <$>
  (foldMap findFn $ findFromRoot val loc)

goToNode :: forall a. Eq a => a -> Loc a -> Loc a
goToNode val loc =
  fromMaybe loc $ findFromRoot val loc

mkLeaf :: forall a. a -> Tree a
mkLeaf val =
  mkTree val mempty

appendChildren :: forall a. List (Tree a) -> Loc a -> Loc a
appendChildren Nil tree = tree
appendChildren (child : rest) loc =
  fromMaybe loc $ up (go rest firstElem)
  where
  firstElem = insertChild child loc
  go Nil elm = elm
  go (child' : rest') elm = go rest' $ insertAfter child' elm
