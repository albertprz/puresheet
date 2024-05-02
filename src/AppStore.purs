module App.AppStore where

import FatPrelude
import Prim hiding (Row)

import App.Components.Spreadsheet.Cell (Cell, CellValue)
import App.Components.Spreadsheet.Formula (Formula, FormulaId)
import App.Evaluator.Common (LocalFormulaCtx)
import App.Interpreter.Module (reloadModule)
import App.Lib.Array (array)
import App.Lib.Matrix (matrix)
import App.Lib.Prelude (prelude)
import App.SyntaxTree.Common (Module, QVar, QVarOp, preludeModule)
import App.SyntaxTree.FnDef (FnInfo, OpInfo)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (toJsonString)
import Data.HashMap as HashMap
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree.Zipper (fromTree)
import Effect.Console as Logger
import Record (merge)
import Record.Extra (pick)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

type StoreRow =
  ( fnsMap :: HashMap QVar FnInfo
  , operatorsMap :: HashMap QVarOp OpInfo
  , aliasedModulesMap :: HashMap (Module /\ Module) (Set Module)
  , importedModulesMap :: HashMap Module (Set Module)
  , modules :: Set Module
  , tableData :: HashMap Cell CellValue
  , tableFormulas :: HashMap Cell FormulaId
  , tableDependencies :: HashMap Cell (NonEmptySet FormulaId)
  , formulaCache :: HashMap FormulaId Formula
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
  , tableData: HashMap.empty
  , tableFormulas: HashMap.empty
  , tableDependencies: HashMap.empty
  , formulaCache: HashMap.empty
  }

reduce :: Store -> StoreAction -> Store
reduce store k = k store

mkLocalContext :: Store -> LocalFormulaCtx
mkLocalContext store =
  pick $ merge store
    { module': preludeModule
    , localFnsMap: HashMap.empty
    , argsMap: HashMap.empty
    , fnInfo: Nothing
    , scope: zero
    , scopeLoc: fromTree $ mkLeaf zero
    , lambdaCount: zero
    }

getCachedStore :: forall m. MonadEffect m => m Store
getCachedStore = do
  store <- getFromLocalStorage
  flip fromMaybe store <$> getInitialStore

getInitialStore :: forall m. MonadEffect m => m Store
getInitialStore = liftEffect do
  (errors /\ newStore) <- runStateT (traverse reloadModule loadedModules)
    emptyStore
  traverse_ logError (foldMap blush errors)
  pure newStore
  where
  loadedModules = [ prelude, matrix, array ]
  logError err = do
    Logger.error
      ("Module load error \n" <> "Parse Error: " <> show err)
    pure emptyStore

getFromLocalStorage :: forall m. MonadEffect m => m (Maybe Store)
getFromLocalStorage = liftEffect do
  storage <- localStorage =<< window
  map (deserializeStore =<< _) $ Storage.getItem storageKey storage

persistInLocalStorage :: forall m. MonadEffect m => Store -> m Unit
persistInLocalStorage store = liftEffect do
  storage <- localStorage =<< window
  Storage.setItem storageKey (serializeStore store) storage

serializeStore :: Store -> String
serializeStore store = toJsonString
  { fnsMap: HashMap.toArrayBy Tuple $ store.fnsMap
  , operatorsMap: HashMap.toArrayBy Tuple $ store.operatorsMap
  , aliasedModulesMap: HashMap.toArrayBy Tuple $ store.aliasedModulesMap
  , importedModulesMap: HashMap.toArrayBy Tuple $ store.importedModulesMap
  , modules: store.modules
  , tableData: HashMap.toArrayBy Tuple $ store.tableData
  , tableFormulas: HashMap.toArrayBy Tuple $ store.tableFormulas
  , tableDependencies: HashMap.toArrayBy Tuple $ map Set.fromFoldable $
      store.tableDependencies
  , formulaCache: HashMap.toArrayBy Tuple
      $ map (\x -> merge { affectedCells: Set.fromFoldable x.affectedCells } x)
          store.formulaCache
  }

deserializeStore :: String -> Maybe Store
deserializeStore = hush <<< map go <<< fromJsonString
  where
  go :: SerialStore -> Store
  go obj =
    { fnsMap: HashMap.fromArray obj.fnsMap
    , operatorsMap: HashMap.fromArray obj.operatorsMap
    , aliasedModulesMap: HashMap.fromArray obj.aliasedModulesMap
    , importedModulesMap: HashMap.fromArray obj.importedModulesMap
    , modules: obj.modules
    , tableData: HashMap.fromArray obj.tableData
    , tableFormulas: HashMap.fromArray obj.tableFormulas
    , tableDependencies: map (unsafeFromJust <<< NonEmptySet.fromSet)
        $ HashMap.fromArray obj.tableDependencies
    , formulaCache:
        map
          ( \x ->
              { affectedCells:
                  unsafeFromJust $ NonEmptySet.fromSet x.affectedCells
              , formulaText: x.formulaText
              , startingCell: x.startingCell
              }
          )
          $ HashMap.fromArray obj.formulaCache
    }

type SerialStore =
  { fnsMap :: Array (QVar /\ FnInfo)
  , operatorsMap :: Array (QVarOp /\ OpInfo)
  , aliasedModulesMap :: Array ((Module /\ Module) /\ (Set Module))
  , importedModulesMap :: Array (Module /\ (Set Module))
  , modules :: Set Module
  , tableData :: Array (Cell /\ CellValue)
  , tableFormulas :: Array (Cell /\ FormulaId)
  , tableDependencies :: Array (Cell /\ (Set FormulaId))
  , formulaCache ::
      Array
        ( FormulaId /\
            { formulaText :: String
            , affectedCells :: Set Cell
            , startingCell :: Cell
            }
        )
  }

storageKey :: String
storageKey = "puresheetTableState"
