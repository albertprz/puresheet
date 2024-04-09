module App.Components.AppStore where

import FatPrelude
import Prim hiding (Row)

import App.Evaluator.Common (LocalFormulaCtx)
import App.SyntaxTree.Common (Module, QVar, QVarOp, preludeModule)
import App.SyntaxTree.FnDef (FnInfo, OpInfo)
import Data.HashMap as HashMap
import Data.Tree.Zipper (fromTree)

type Store =
  { fnsMap :: HashMap QVar FnInfo
  , operatorsMap :: HashMap QVarOp OpInfo
  , aliasedModulesMap :: HashMap (Module /\ Module) (Set Module)
  , importedModulesMap :: HashMap Module (Set Module)
  }

type StoreAction = Store -> Store

initialStore :: Store
initialStore =
  { fnsMap: HashMap.empty
  , operatorsMap: HashMap.empty
  , aliasedModulesMap: HashMap.empty
  , importedModulesMap: HashMap.empty
  }

reduce :: Store -> StoreAction -> Store
reduce store k = k store

mkLocalContext :: Store -> LocalFormulaCtx
mkLocalContext store =
  { tableData: HashMap.empty
  , fnsMap: store.fnsMap
  , operatorsMap: store.operatorsMap
  , aliasedModulesMap: store.aliasedModulesMap
  , importedModulesMap: store.importedModulesMap
  , localFnsMap: HashMap.empty
  , argsMap: HashMap.empty
  , module': preludeModule
  , scope: zero
  , scopeLoc: fromTree $ mkLeaf zero
  , lambdaCount: zero
  }
