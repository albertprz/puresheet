module App.Evaluator.Expression where

import FatPrelude

import App.Evaluator.Builtins as Builtins
import App.Evaluator.Common (EvalM, LocalFormulaCtx, argId, lookupFn, lookupOperator, registerBindings, registerLocalFn, varFn)
import App.Evaluator.Errors (EvalError(..), LexicalError(..), MatchError(..), TypeError(..), raiseError)
import App.Evaluator.Object (cellValueToObj, extractBool, extractNList)
import App.SyntaxTree.Common (Var(..), VarOp(..))
import App.SyntaxTree.FnDef (Arity(..), Associativity(..), BuiltinFnInfo, CaseBinding(..), FnBody(..), FnDef(..), FnInfo, FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), Object(..), OpInfo, PatternGuard(..))
import App.SyntaxTree.Pattern (Pattern(..))
import App.Utils.Map (lookupArray) as Map
import Bookhound.FatPrelude (hasSome)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Monad.Except (except, runExceptT)
import Data.Map (fromFoldable, lookup, member, union) as Map
import Data.Set as Set

evalExpr :: FnBody -> EvalM Object

evalExpr (FnApply fnExpr args) = do
  fnObj <- evalExpr fnExpr
  argObjs <- traverse (\x -> evalExpr x) args
  case fnObj of
    FnObj fnInfo -> evalFn fnInfo (Object' <$> argObjs)
    BuiltinFnObj fnInfo -> evalBuiltinFn fnInfo argObjs
    _ -> raiseError $ TypeError' $ NotAFunction fnObj

evalExpr (InfixFnApply fnOps args) =
  do
    { operatorsMap } <- get
    let
      noteUnknownOperator = except <<< note
        ( LexicalError' $ UnknownOperator $ fromMaybe (VarOp "")
            unknownOperator
        )
      unknownOperator = find (not <<< (_ `Map.member` operatorsMap)) fnOps
    nestedExpr <- noteUnknownOperator
      ((_ `nestInfixFns` args) =<< Map.lookupArray fnOps operatorsMap)
    evalExpr nestedExpr

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
  except $
    findMapEither (MatchError' NonExhaustiveMatch)
      (evalGuardedFnBody st)
      conds

evalExpr (SwitchExpr matchee cases) = do
  st <- get
  result <- evalExpr matchee
  except $ findMapEither (MatchError' NonExhaustiveMatch)
    (evalCaseBinding st result)
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
  else raiseError $ TypeError' $ TooManyArguments $ length args
  where
  unappliedArgsNum = length params - length args
  argBindings = Map.fromFoldable
    $ rmap (\arg -> { body: arg, params: [], scope: scope })
    <$> zip' ((\x -> (scope /\ x)) <$> params) args

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
  :: Array (VarOp /\ OpInfo)
  -> Array FnBody
  -> Maybe FnBody
nestInfixFns [ (_ /\ { fnName }) ] args =
  pure $ FnApply (FnVar' $ Var' fnName) args

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
      $ updateAt' idx (FnApply (FnVar' $ Var' fnName) redexArgs) args
  nestInfixFns newFns newArgs

evalCaseBinding
  :: LocalFormulaCtx
  -> Object
  -> CaseBinding
  -> Either EvalError Object
evalCaseBinding ctx matchee (CaseBinding pattern body) =
  evalState action ctx
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
evalGuardedFnBody ctx (GuardedFnBody guard body) =
  evalState action ctx
  where
  action = runExceptT
    $ ifM (evalGuard guard)
        (evalExpr body)
        (raiseError $ MatchError' NonExhaustiveMatch)

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

evalPatternBinding (ArrayPattern patterns) result
  | Just idx <- findIndex' (_ == Spread) patterns
  , length (filter (_ == Spread) patterns) == 1
  , ArrayObj results <- result =
      evalPatternBinding
        (ArrayPattern $ patternsBegin <> patternsEnd)
        (ArrayObj $ resultsBegin <> resultsEnd)
      where
      { before, after } = splitAt' idx patterns
      (patternsBegin /\ patternsEnd) = (before /\ fold (tail' after))
      resultsBegin = take' (length patternsBegin) results
      resultsEnd = takeEnd' (length patternsEnd) results

evalPatternBinding (ArrayPattern patterns) result
  | Just results <- extractNList (length patterns) result =
      and <$> traverse (\(x /\ y) -> evalPatternBinding x y)
        (patterns `zip'` results)

evalPatternBinding (ArrayPattern _) _ =
  lift $ pure false

evalPatternBinding Wildcard _ =
  pure true

evalPatternBinding Spread _ =
  pure true
