module App.Interpreters.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Components.Table.Models (AppState)
import App.SyntaxTrees.Common (Literal(..), VarOp)
import App.SyntaxTrees.FnDef (FnBody(..), FnVar(..))
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Filterable (filterMap)
import Data.Map as Map
import Partial.Unsafe (unsafeCrashWith)

evalExpr :: forall m. MonadState AppState m => FnBody -> m (Maybe Literal)
evalExpr = map extractLiteral <<< reduceExpr

reduceExpr :: forall m. MonadState AppState m => FnBody -> m FnBody
reduceExpr (FnApply (FnVar' (Var' fnName)) args)
  | Just args' <- traverse extractLiteral args =
      do
        (f /\ arity) <- lookupFn fnName (_.fnsMap)
        if arity == length args' then
          pure $ Literal' $ f args'
        else unsafeCrashWith "Partially applied Fn"

reduceExpr (FnApply fn args) =
  reduceExpr =<< (FnApply <$> reduceExpr fn <*> traverse reduceExpr args)

reduceExpr (InfixFnApply fns args) =
  do
    { precedenceMap } <- get
    reduceExpr =<< flattenInfixFns (lookupArray fns precedenceMap) args

reduceExpr (Cell' x) = Literal' <$> fromCell x
reduceExpr x = pure x

flattenInfixFns
  :: forall m
   . MonadState AppState m
  => Array (VarOp /\ Int)
  -> Array FnBody
  -> m FnBody
flattenInfixFns [ (fn /\ _) ] args = do
  fnVar <- lookupFn fn (_.operatorsMap)
  pure $ FnApply (FnVar' $ Var' fnVar) args

flattenInfixFns fns args =
  do
    let (fn /\ _) = unsafeFromJust $ maximumBy (compare `on` snd) fns
    { rAssocSet } <- get
    fnVar <- lookupFn fn (_.operatorsMap)
    let
      (indexFn /\ sliceFn) =
        if elem fn rAssocSet then (findLastIndex' /\ slicePrev')
        else (findIndex' /\ sliceNext')
      idx = unsafeFromJust $ indexFn ((_ == fn) <<< fst) fns
      newFns = fold $ deleteAt' idx fns
      redexArgs = sliceFn 2 idx args
      newArgs = fold $ deleteAt' (idx + 1) $ fold
        $ updateAt' idx (FnApply (FnVar' $ Var' fnVar) redexArgs) args
    flattenInfixFns newFns newArgs

fromCell :: forall m. MonadState AppState m => Cell -> m Literal
fromCell cell = do
  { tableData } <- get
  pure $ maybe (ListLit []) fromCellValue $ Map.lookup cell tableData
  where
  fromCellValue (BoolVal x) = BoolLit x
  fromCellValue (IntVal x) = IntLit x
  fromCellValue (FloatVal x) = FloatLit x
  fromCellValue (StringVal x) = StringLit x

lookupFn
  :: forall k v m
   . MonadState AppState m
  => Show k
  => Ord k
  => k
  -> (AppState -> Map k v)
  -> m v
lookupFn fnName fetchFnMap =
  fromMaybe (unsafeCrashWith $ "Unknown function: " <> show fnName)
    <$> Map.lookup fnName
    <$> gets fetchFnMap

lookupArray :: forall k v. Ord k => Array k -> Map k v -> Array (k /\ v)
lookupArray keys dict = keys `zip'` vals
  where
  vals = filterMap (_ `Map.lookup` dict) keys

extractLiteral :: FnBody -> Maybe Literal
extractLiteral (Literal' x) = Just x
extractLiteral _ = Nothing
