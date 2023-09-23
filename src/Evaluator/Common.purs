module App.Evaluator.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Evaluator.Errors (EvalError(..), LexicalError(..))
import App.SyntaxTree.Common (Var(..), VarOp)
import App.SyntaxTree.FnDef (FnBody(..), FnDef(..), FnInfo, FnVar(..), OpInfo, Scope(..))
import Bookhound.FatPrelude (findJust)
import Control.Monad.Except (ExceptT, except)
import Data.List as List
import Data.Map as Map
import Data.Tree.Zipper (Loc, toTree)

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map (Scope /\ Var) FnInfo
  , operatorsMap :: Map VarOp OpInfo
  , argsMap :: Map (Scope /\ Var) FnInfo
  , scope :: Scope
  , scopeLoc :: Loc Scope
  }

type EvalM a = forall m. MonadState LocalFormulaCtx m => ExceptT EvalError m a

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
-- 4. Free variables (Closed over bindings + args)
lookupFn :: Var -> EvalM FnInfo
lookupFn fnName = do
  { fnsMap, argsMap, scope: scope, scopeLoc } <- get
  let
    lookupVar n = Map.lookup (n /\ fnName) fnsMap
    lookupArg n = Map.lookup (n /\ fnName) argsMap
    lookupVarOrArg n = Map.lookup (n /\ fnName) (Map.union fnsMap argsMap)
    childrenLookup = lookupVar <$> childrenValues scope scopeLoc
    siblingsLookup = lookupVar <$> siblingsValues scope scopeLoc
    argsLookup = lookupArg <$> nodeValues scope scopeLoc
    freeVarsLookup = lookupVarOrArg <$> ancestorsValues scope scopeLoc
  except
    $ note (LexicalError' $ UnknownValue fnName)
    $ findJust
        (childrenLookup <> argsLookup <> siblingsLookup <> freeVarsLookup)

lookupOperator :: VarOp -> EvalM OpInfo
lookupOperator opName = do
  { operatorsMap } <- get
  except
    $ note (LexicalError' $ UnknownOperator opName)
    $ Map.lookup opName operatorsMap

nonEmptyCellValue :: CellValue -> Boolean
nonEmptyCellValue (StringVal "") = false
nonEmptyCellValue _ = true

varFn :: String -> FnBody
varFn = FnVar' <<< Var' <<< Var

argId :: String
argId = "__arg__"
