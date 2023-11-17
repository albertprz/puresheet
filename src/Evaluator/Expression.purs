module App.Evaluator.Expression where

import FatPrelude

import App.Evaluator.Common (EvalM, LocalFormulaCtx, extractAlias, getNewFnState, isSpread, lambdaId, lookupBuiltinFn, lookupFn, lookupOperator, registerArg, registerBindings, resetFnScope, substituteFnArgs, varFn)
import App.Evaluator.Errors (EvalError(..), MatchError(..), TypeError(..), raiseError)
import App.Evaluator.Object (cellValueToObj, extractBool, extractNList)
import App.SyntaxTree.Common (QVar(..), Var(..), preludeModule)
import App.SyntaxTree.FnDef (Associativity(..), BuiltinFnInfo, CaseBinding(..), FnBody(..), FnDef(..), FnInfo(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), Object(..), OpInfo, PatternGuard(..))
import App.SyntaxTree.Pattern (Pattern(..))
import Data.Array as Array
import Data.HashMap as HashMap
import Data.Set as Set
import Data.Tree.Zipper (insertChild, toTree)

evalExpr :: FnBody -> EvalM Object

evalExpr (FnApply fnExpr args) = do
  argObjs <- traverse (\x -> evalExpr x) args
  fnObj <- evalExpr fnExpr
  case fnObj of
    FnObj fnInfo -> evalFn fnInfo (Object' <$> argObjs)
    BuiltinFnObj fnInfo -> evalBuiltinFn fnInfo argObjs
    _ -> raiseError $ TypeError' $ NotAFunction fnObj

evalExpr (LambdaFn params body) = do
  { lambdaCount } <- modify \st -> st { lambdaCount = inc st.lambdaCount }
  let lambdaVar = Var $ lambdaId lambdaCount
  evalExpr $ WhereExpr (FnVar $ QVar Nothing lambdaVar)
    [ FnDef lambdaVar ((_ /\ Nothing) <$> params) Nothing body ]

evalExpr (InfixFnApply fnOps args) = do
  opInfos <- traverse (\x -> lookupOperator x) fnOps
  nestedExpr <- pure $ unsafeFromJust $ flip nestInfixFns args opInfos
  evalExpr nestedExpr

evalExpr (LeftOpSection fnOp body) = do
  { lambdaCount } <- modify \st -> st { lambdaCount = inc st.lambdaCount }
  let lambdaVar = Var $ lambdaId lambdaCount
  evalExpr $ LambdaFn [ lambdaVar ]
    (InfixFnApply [ fnOp ] [ FnVar $ QVar Nothing lambdaVar, body ])

evalExpr (RightOpSection body fnOp) = do
  { lambdaCount } <- modify \st -> st { lambdaCount = inc st.lambdaCount }
  let lambdaVar = Var $ lambdaId lambdaCount
  evalExpr $ LambdaFn [ lambdaVar ]
    (InfixFnApply [ fnOp ] [ body, FnVar $ QVar Nothing lambdaVar ])

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

evalExpr (FnVar qVar@(QVar module' var))
  | module' == Nothing || module' == Just preludeModule =
      getBuiltinFnInfo var <|> getFnInfo qVar

evalExpr (FnVar fn) =
  getFnInfo fn

evalExpr (FnOp fnOp) = do
  { fnName } <- lookupOperator fnOp
  evalExpr (FnVar fnName)

evalExpr (Cell' cell) =
  do
    { tableData } <- get
    pure $ maybe NullObj cellValueToObj $ HashMap.lookup cell
      tableData

evalExpr (CellValue' cellValue) = pure $ cellValueToObj cellValue

evalExpr (Object' (FnObj fnInfo@(FnInfo { params: [] }))) =
  evalFn fnInfo []

evalExpr (Object' (BuiltinFnObj fnInfo@{ params: [] })) =
  evalBuiltinFn fnInfo []

evalExpr (Object' obj) = pure obj

evalFn :: FnInfo -> Array FnBody -> EvalM Object
evalFn (FnInfo fnInfo@{ body, params, id: maybeFnId }) args = do

  st <- get
  newSt <- getNewFnState (FnInfo fnInfo) args

  if unappliedArgsNum == 0 then do
    put newSt
    result <- evalExpr body
    newScopeLoc <- gets _.scopeLoc
    put st
    if isJust maybeFnId then
      pure $ resetFnScope
        $ substituteFnArgs result (map fst params `zip'` args)
    else
      modify_ _ { scopeLoc = newScopeLoc } *> pure result

  else if unappliedArgsNum > 0 then
    pure $ FnObj $ FnInfo $ fnInfo
      { params = takeEnd' unappliedArgsNum params
      , argsMap = newSt.argsMap
      }

  else do
    let
      { before: preArgs, after: postArgs } =
        splitAt' (length params) args
    fn <- evalFn (FnInfo fnInfo) preArgs
    evalExpr $ FnApply (Object' fn) postArgs

  where
  unappliedArgsNum = length params - length args

evalBuiltinFn :: BuiltinFnInfo -> Array Object -> EvalM Object
evalBuiltinFn fnInfo@{ fn, params, defaultParams } args =

  if unappliedArgsNum == 0 then
    except
      $ note (TypeError' $ InvalidArgumentTypes args)
      $ fromMaybe' (\_ -> partialMaybe fn args) (pure <$> defaultResult)

  else if unappliedArgsNum > 0 then
    pure $ BuiltinFnObj $ fnInfo
      { fn = \newArgs -> fn (args <> newArgs)
      , params = takeEnd' unappliedArgsNum params
      , defaultParams = Set.filter zeroOrPos
          $ Set.map (_ - length args) defaultParams
      }

  else
    raiseError $ TypeError' $ TooManyArguments $ length args

  where
  unappliedArgsNum = length params - length args
  defaultResult =
    if elem NullObj args && not (Set.isEmpty defaultParams) then
      find
        (_ /= NullObj)
        (filterByIndexes defaultParams args)
    else
      Nothing

nestInfixFns
  :: Array OpInfo
  -> Array FnBody
  -> Maybe FnBody
nestInfixFns [ { fnName } ] args =
  pure $ FnApply (FnVar fnName) args

nestInfixFns fnOps args = do
  ({ associativity, precedence }) <-
    maximumBy (compare `on` _.precedence) fnOps
  let
    indexFn =
      case associativity of
        L -> findIndex'
        R -> findLastIndex'
  idx <- indexFn
    ( \x -> x.associativity == associativity
        && (x.precedence == precedence)
    )
    fnOps
  { fnName } <- index' fnOps idx
  let
    newFns = fold $ deleteAt' idx fnOps
    redexArgs = sliceNext' 2 idx args
    newArgs = fold $ deleteAt' (idx + 1)
      $ fold
      $ updateAt' idx (FnApply (FnVar fnName) redexArgs) args
  nestInfixFns newFns newArgs

evalCaseBinding
  :: LocalFormulaCtx
  -> Object
  -> CaseBinding
  -> Either EvalError Object
evalCaseBinding st matchee (CaseBinding pattern body) =
  flip evalState st
    $ runExceptT
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
  flip evalState st
    $ runExceptT
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
evalPatternBinding (VarPattern param) result = do
  { scope } <- get
  registerArg scope (FnDef param [] Nothing $ Object' result) $> true

evalPatternBinding (LitPattern cellValue) result =
  pure $ cellValueToObj cellValue == result

evalPatternBinding (AliasedPattern param pattern) result = do
  { scope } <- get
  registerArg scope (FnDef param [] Nothing $ Object' result) *>
    evalPatternBinding pattern result

evalPatternBinding (ArrayPattern patterns) result
  | [ headPat, Spread ] <- patterns
  , ListObj (resultsHead : _) <- result =
      evalPatternBinding headPat resultsHead

  | [ headPat, AliasedPattern tailPat Spread ] <- patterns
  , ListObj (resultsHead : resultsTail) <- result =
      evalPatternBinding headPat resultsHead *>
        evalPatternBinding (VarPattern tailPat) (ListObj resultsTail)

evalPatternBinding pattern@(ArrayPattern _) (ListObj xs) =
  evalPatternBinding pattern (ArrayObj $ Array.fromFoldable xs)

evalPatternBinding (ArrayPattern patterns) result
  | Just idx <- findIndex' isSpread patterns
  , length (filter isSpread patterns) == 1
  , ArrayObj results <- result =
      do
        void $ case findMap extractAlias patterns of
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
      and <$> traverse (\x -> uncurry evalPatternBinding x)
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
    FnInfo { body: FnApply _ _ } -> evalFn fnInfo [] <|> evalExpr
      (Object' $ FnObj $ fnInfo)
    _ -> evalExpr (Object' $ FnObj $ fnInfo)

getBuiltinFnInfo :: Var -> EvalM Object
getBuiltinFnInfo fnName =
  BuiltinFnObj <$> lookupBuiltinFn fnName
