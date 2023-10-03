module App.Evaluator.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Evaluator.Errors (EvalError(..), LexicalError(..))
import App.SyntaxTree.Common (Module, QVar(..), QVarOp, Var(..))
import App.SyntaxTree.FnDef (FnBody(..), FnDef(..), FnInfo, OpInfo, Scope(..))
import Bookhound.FatPrelude (findJust)
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Tree.Zipper (Loc, toTree)

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map QVar FnInfo
  , operatorsMap :: Map QVarOp OpInfo
  , aliasedModulesMap :: Map (Module /\ Module) (Set Module)
  , importedModulesMap :: Map Module (Set Module)
  , localFnsMap :: Map (Scope /\ Var) FnInfo
  , argsMap :: Map (Scope /\ Var) FnInfo
  , module' :: Module
  , scope :: Scope
  , scopeLoc :: Loc Scope
  }

type EvalM a = forall m. MonadState LocalFormulaCtx m => ExceptT EvalError m a

registerBindings :: Array FnDef -> EvalM Unit
registerBindings bindings = do
  { scope, scopeLoc } <- get
  let (Scope maxScope) = fromMaybe scope $ maximum $ toTree scopeLoc
  let scopes = Scope <<< (_ + maxScope) <$> (1 .. length bindings)
  traverse_ (\(n /\ x) -> registerLocalFn n x) (scopes `zip'` bindings)
  modify_ \st -> st
    { scopeLoc = appendChildren (mkLeaf <$> List.fromFoldable scopes)
        st.scopeLoc
    }

registerLocalFn :: Scope -> FnDef -> EvalM Unit
registerLocalFn newScope (FnDef fnName params body) =
  modify_ \st ->
    st
      { localFnsMap = Map.insert (newScope /\ fnName)
          { params, body, scope: newScope }
          st.localFnsMap
      }

lookupFn :: QVar -> EvalM FnInfo
lookupFn qVar@(QVar Nothing var) =
  lookupLocalFn var <|> lookupModuleFn qVar

lookupFn qVar = lookupModuleFn qVar

-- Scope resolution:
-- 1. Children bindings
-- 2. Fn args
-- 3. Siblings bindings
-- 4. Free variables (Closed over bindings + args)
lookupLocalFn :: Var -> EvalM FnInfo
lookupLocalFn fnName = do
  { localFnsMap, argsMap, scope, scopeLoc } <- get
  let
    lookupVar n = Map.lookup (n /\ fnName) localFnsMap
    lookupArg n = Map.lookup (n /\ fnName) argsMap
    lookupVarOrArg n = Map.lookup (n /\ fnName) (Map.union localFnsMap argsMap)
    childrenLookup = lookupVar <$> childrenValues scope scopeLoc
    siblingsLookup = lookupVar <$> siblingsValues scope scopeLoc
    argsLookup = lookupArg <$> nodeValues scope scopeLoc
    freeVarsLookup = lookupVarOrArg <$> ancestorsValues scope scopeLoc
  except
    $ note (LexicalError' $ UnknownValue $ QVar Nothing fnName)
    $ findJust
        (childrenLookup <> argsLookup <> siblingsLookup <> freeVarsLookup)

lookupModuleFn :: QVar -> EvalM FnInfo
lookupModuleFn qVar@(QVar maybeMod fnName) = do
  { module', importedModulesMap, aliasedModulesMap, fnsMap } <- get
  let
    modules = case maybeMod of
      Just mod -> fromMaybe Set.empty $ Map.lookup (module' /\ mod)
        aliasedModulesMap
      Nothing -> fromMaybe Set.empty $ Map.lookup module' importedModulesMap
    fns = flip QVar fnName <<< pure <$> Array.fromFoldable modules
  except
    $ note (LexicalError' $ UnknownValue qVar)
    $ findJust
    $ flip Map.lookup fnsMap
    <$> fns

lookupOperator :: QVarOp -> EvalM OpInfo
lookupOperator opName = do
  { operatorsMap } <- get
  except
    $ note (LexicalError' $ UnknownOperator opName)
    $ Map.lookup opName operatorsMap

nonEmptyCellValue :: CellValue -> Boolean
nonEmptyCellValue (StringVal "") = false
nonEmptyCellValue _ = true

varFn :: String -> FnBody
varFn = FnVar <<< QVar Nothing <<< Var

argId :: String
argId = "__arg__"
