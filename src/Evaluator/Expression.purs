module App.Evaluator.Expression where

import FatPrelude

import App.Evaluator.Builtins as Builtins
import App.Evaluator.Common (EvalM, LocalFormulaCtx, argId, extractAlias, isSpread, lookupFn, lookupOperator, registerArg, registerBindings, varFn)
import App.Evaluator.Errors (EvalError(..), LexicalError(..), MatchError(..), TypeError(..), raiseError)
import App.Evaluator.Object (cellValueToObj, extractBool, extractNList)
import App.SyntaxTree.Common (QVar(..), QVarOp(..), Var(..), VarOp(..))
import App.SyntaxTree.FnDef (Arity(..), Associativity(..), BuiltinFnInfo, CaseBinding(..), FnBody(..), FnDef(..), FnInfo(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), Object(..), OpInfo, PatternGuard(..))
import App.SyntaxTree.Pattern (Pattern(..))
import App.Utils.Map (lookupArray) as Map
import Bookhound.FatPrelude (hasSome)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Alternative ((<|>))
import Data.Map (empty, fromFoldable, lookup, member, union) as Map
import Data.Set as Set
import Data.Tree.Zipper (fromTree, insertChild, toTree)

evalExpr :: FnBody -> EvalM Object

evalExpr (FnApply fnExpr args) = do
  fnObj <- evalExpr fnExpr
  argObjs <- traverse (\x -> evalExpr x) args
  case fnObj of
    FnObj fnInfo -> evalFn fnInfo (Object' <$> argObjs)
    BuiltinFnObj fnInfo -> evalBuiltinFn fnInfo argObjs
    _ -> raiseError $ TypeError' $ NotAFunction fnObj

evalExpr (LambdaFn params body) =
  evalExpr $ WhereExpr (FnVar $ QVar Nothing $ Var argId)
    [ FnDef (Var argId) params body ]

evalExpr (InfixFnApply fnOps args) =
  do
    { operatorsMap } <- get
    let
      noteUnknownOperator = except <<< note
        ( LexicalError' $ UnknownOperator $ fromMaybe
            (QVarOp Nothing $ VarOp "")
            unknownOperator
        )
      unknownOperator = find (not <<< (_ `Map.member` operatorsMap)) fnOps
    nestedExpr <- noteUnknownOperator
      (flip nestInfixFns args =<< Map.lookupArray fnOps operatorsMap)
    evalExpr nestedExpr

evalExpr (LeftOpSection fnOp body) = do
  { scope } <- get
  pure $ FnObj
    $ FnInfo
        { id: Nothing
        , body: (InfixFnApply [ fnOp ] [ body, varFn argId ])
        , params: [ Var argId ]
        , scope
        , argsMap: Map.empty
        }

evalExpr (RightOpSection body fnOp) = do
  { scope } <- get
  pure $ FnObj
    $ FnInfo
        { id: Nothing
        , body: (InfixFnApply [ fnOp ] [ varFn argId, body ])
        , params: [ Var argId ]
        , scope
        , argsMap: Map.empty
        }

evalExpr (WhereExpr fnBody bindings) =
  registerBindings bindings *> evalExpr fnBody

evalExpr (CondExpr conds) = do
  st <- get
  let
    newScope = inc $ fromMaybe st.scope $ maximum $ toTree st.scopeLoc
    newSt = st
      { scopeLoc = insertChild (mkLeaf newScope) st.scopeLoc
      , scope = newScope
      }
  except $
    findMapEither (MatchError' NonExhaustiveGuard)
      (evalGuardedFnBody newSt)
      conds

evalExpr (SwitchExpr matchee cases) = do
  result <- evalExpr matchee
  st <- get
  let
    newScope = inc $ fromMaybe st.scope $ maximum $ toTree st.scopeLoc
    newSt = st
      { scopeLoc = insertChild (mkLeaf newScope) st.scopeLoc
      , scope = newScope
      }
  except $ findMapEither (MatchError' NonExhaustiveMatch)
    (evalCaseBinding newSt result)
    cases

evalExpr
  ( CellMatrixRange { column: colX, row: rowX }
      { column: colY, row: rowY }
  ) =
  evalExpr $ Array' (Array' <$> matrix)
  where
  matrix = do
    row <- rowX .. rowY
    pure $ do
      column <- colX .. colY
      pure $ Cell' { column, row }

evalExpr
  ( CellArrayRange x@{ column: colX, row: rowX }
      y@{ column: colY, row: rowY }
  )
  | rowX == rowY =
      evalExpr $ Array' $ Cell' <<< { column: _, row: rowX }
        <$> (colX .. colY)
  | colX == colY =
      evalExpr $ Array' $ Cell' <<< { column: colX, row: _ }
        <$> (rowX .. rowY)
  | otherwise =
      raiseError $ TypeError' $ InvalidCellArrayRange x y

evalExpr (ArrayRange x y) = evalExpr $ FnApply (varFn "range") [ x, y ]

evalExpr (Array' array) =
  evalExpr
    $ foldl (FnApply (varFn "snoc") <.. arr2)
        (Object' $ ArrayObj [])
        array

evalExpr (FnVar fn) =
  getFnInfo fn <|> getBuiltinFnInfo fn

evalExpr (FnOp fnOp) = do
  { fnName } <- lookupOperator fnOp
  evalExpr (FnVar fnName)

evalExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ maybe NullObj cellValueToObj $ Map.lookup cell
      tableData

evalExpr (CellValue' cellValue) = pure $ cellValueToObj cellValue

evalExpr (Object' (FnObj fnInfo@(FnInfo { params: [] }))) =
  evalFn fnInfo []

evalExpr (Object' (BuiltinFnObj fnInfo@{ arity: A0 })) =
  evalBuiltinFn fnInfo []

evalExpr (Object' obj) = pure obj

evalFn
  :: FnInfo
  -> Array FnBody
  -> EvalM Object
evalFn (FnInfo { body, params, scope, id: fnId, argsMap: fnArgsMap }) args = do
  st <- get
  let
    argsMap = Map.union fnArgsMap $ Map.union argBindings st.argsMap
    newSt = case fnId of
      Just { fnModule } -> st
        { argsMap = argsMap
        , module' = fnModule
        , scope = zero
        , scopeLoc = fromTree $ mkLeaf zero
        }
      Nothing -> st
        { argsMap = argsMap
        , scope = scope
        , scopeLoc = goToNode scope st.scopeLoc
        }
  if unappliedArgsNum == 0 then do
    put newSt
    result <- evalExpr body
    newScopeLoc <- gets _.scopeLoc
    put st
    when (isNothing fnId)
      (modify_ _ { scopeLoc = newScopeLoc })
    pure result
  else if unappliedArgsNum > 0 then
    pure
      $ FnObj
      $ FnInfo
          { id: Nothing
          , body
          , params: takeEnd' unappliedArgsNum params
          , scope
          , argsMap
          }
  else raiseError $ TypeError' $ TooManyArguments $ length args
  where
  unappliedArgsNum = length params - length args
  argBindings = Map.fromFoldable
    $ rmap
        ( \arg ->
            FnInfo
              { id: Nothing
              , body: arg
              , scope
              , params: []
              , argsMap: Map.empty
              }
        )
    <$> zip' ((scope /\ _) <$> params) args

evalBuiltinFn :: BuiltinFnInfo -> Array Object -> EvalM Object
evalBuiltinFn { fn, arity, defaultParams } args =
  if unappliedArgsNum == 0 then
    except
      $ note (TypeError' $ InvalidArgumentTypes args)
      $ fromMaybe' (\_ -> partialMaybe fn args) (pure <$> defaultResult)
  else if unappliedArgsNum > 0 then
    pure $ BuiltinFnObj
      { fn: \newArgs -> fn (args <> newArgs)
      , arity: unsafeFromJust $ toEnum unappliedArgsNum
      , defaultParams: Set.filter zeroOrPos
          $ Set.map (_ - length args) defaultParams
      }
  else
    raiseError $ TypeError' $ TooManyArguments $ length args
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
  :: Array (QVarOp /\ OpInfo)
  -> Array FnBody
  -> Maybe FnBody
nestInfixFns [ (_ /\ { fnName }) ] args =
  pure $ FnApply (FnVar fnName) args

nestInfixFns fnOps args = do
  (fnOp /\ { fnName, associativity }) <- maximumBy
    (compare `on` (_.precedence <<< snd))
    fnOps
  let
    indexFn =
      case associativity of
        L -> findIndex'
        R -> findLastIndex'
  idx <- indexFn ((_ == fnOp) <<< fst) fnOps
  let
    newFns = fold $ deleteAt' idx fnOps
    redexArgs = sliceNext' 2 idx args
    newArgs = fold $ deleteAt' (idx + 1) $ fold
      $ updateAt' idx (FnApply (FnVar fnName) redexArgs) args
  nestInfixFns newFns newArgs

evalCaseBinding
  :: LocalFormulaCtx
  -> Object
  -> CaseBinding
  -> Either EvalError Object
evalCaseBinding st matchee (CaseBinding pattern body) =
  evalState action st
  where
  action = runExceptT
    $ ifM (evalPatternBinding pattern matchee)
        (evalMaybeGuardedFnBody body)
        (raiseError $ MatchError' NonExhaustiveMatch)

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
  -> Either EvalError Object
evalGuardedFnBody st (GuardedFnBody guard body) =
  evalState action st
  where
  action = runExceptT
    $ ifM (evalGuard guard)
        (evalExpr body)
        (raiseError $ MatchError' NonExhaustiveGuard)

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
  registerArg scope (FnDef var [] $ Object' result) $> true

evalPatternBinding (LitPattern cellValue) result =
  pure $ cellValueToObj cellValue == result

evalPatternBinding (AliasedPattern var pattern) result = do
  { scope } <- get
  registerArg scope (FnDef var [] $ Object' result) *>
    evalPatternBinding pattern result

evalPatternBinding (ArrayPattern patterns) result
  | Just idx <- findIndex' isSpread patterns
  , length (filter isSpread patterns) == 1
  , ArrayObj results <- result =
      do
        _ <- case findMap extractAlias patterns of
          Just alias -> evalPatternBinding (VarPattern alias)
            (ArrayObj resultsBetween)
          Nothing -> pure true
        evalPatternBinding
          (ArrayPattern $ patternsBegin <> patternsEnd)
          (ArrayObj $ resultsBegin <> resultsEnd)
      where
      { before, after } = splitAt' idx patterns
      (patternsBegin /\ patternsEnd) = (before /\ fold (tail' after))
      resultsBegin = take' (length patternsBegin) results
      resultsBetween = slice' (length patternsBegin)
        (length results - length patternsEnd)
        results
      resultsEnd = takeEnd' (length patternsEnd) results

evalPatternBinding (ArrayPattern patterns) result
  | Just results <- extractNList (length patterns) result =
      and <$> traverse (\(x /\ y) -> evalPatternBinding x y)
        (patterns `zip'` results)
  | otherwise = pure false

evalPatternBinding Wildcard _ =
  pure true

evalPatternBinding Spread _ =
  pure false

getFnInfo :: QVar -> EvalM Object
getFnInfo fnName = do
  fnInfo <- lookupFn fnName
  case fnInfo of
    FnInfo { body: FnApply _ _ } -> evalFn fnInfo []
    _ -> evalExpr $ Object' $ FnObj $ fnInfo

getBuiltinFnInfo :: QVar -> EvalM Object
getBuiltinFnInfo fnName =
  except
    $ note (LexicalError' $ UnknownValue fnName)
    $ BuiltinFnObj
    <$> Map.lookup fnName Builtins.builtinFnsMap
