module App.AppStore where

import FatPrelude
import Prim hiding (Row)

import App.Evaluator.Common (LocalFormulaCtx)
import App.Interpreter.Module (reloadModule)
import App.SyntaxTree.Common (Module, QVar, QVarOp, preludeModule)
import App.SyntaxTree.FnDef (FnInfo, OpInfo)
import Data.HashMap as HashMap
import Data.Set as Set
import Data.Tree.Zipper (fromTree)
import Effect.Console as Logger
import Foreign (readArray, readString, unsafeToForeign)
import Foreign.Index ((!))
import Record (merge)
import Web.HTML (window)

type StoreRow =
  ( fnsMap :: HashMap QVar FnInfo
  , operatorsMap :: HashMap QVarOp OpInfo
  , aliasedModulesMap :: HashMap (Module /\ Module) (Set Module)
  , importedModulesMap :: HashMap Module (Set Module)
  , modules :: Set Module
  )

type Store = Record StoreRow

type StoreAction = Store -> Store

emptyStore :: Store
emptyStore =
  { fnsMap: HashMap.empty
  , operatorsMap: HashMap.empty
  , aliasedModulesMap: HashMap.empty
  , importedModulesMap: HashMap.empty
  , modules: Set.empty
  }

reduce :: Store -> StoreAction -> Store
reduce store k = k store

mkLocalContext :: Store -> LocalFormulaCtx
mkLocalContext store =
  merge store
    { tableData: HashMap.empty
    , localFnsMap: HashMap.empty
    , argsMap: HashMap.empty
    , module': preludeModule
    , scope: zero
    , scopeLoc: fromTree $ mkLeaf zero
    , lambdaCount: zero
    }

getInitialStore
  :: forall m. MonadEffect m => m Store
getInitialStore = liftEffect do
  loadedModules <- getLoadedModules
  (errors /\ newStore) <- runStateT (traverse reloadModule loadedModules)
    emptyStore
  traverse_ logError (foldMap blush errors)
  pure newStore
  where
  logError err = do
    Logger.error
      ("Module load error \n" <> "Parse Error: " <> show err)
    pure emptyStore
  getLoadedModules =
    map (fromRight mempty)
      $ runExceptT
      $ traverse readString
      =<< readArray
      =<< (_ ! "loadedModules")
      =<< unsafeToForeign
      <$> liftEffect window
