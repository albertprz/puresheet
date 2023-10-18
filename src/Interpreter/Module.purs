module App.Interpreter.Module (reloadModule) where

import FatPrelude

import App.Parser.ModuleDef (moduleDef)
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..))
import App.SyntaxTree.FnDef (FnDef(..), FnInfo(..), OpDef(..), OpInfo)
import App.SyntaxTree.ModuleDef (ModuleDef(..), ModuleImport(..))
import App.Utils.Map (deleteWhen) as Map
import Bookhound.Parser (ParseError, runParser)
import Data.Map (delete, empty, fromFoldable, insertWith, union) as Map
import Data.Set as Set

type RegisterModuleCtx r =
  { fnsMap :: Map QVar FnInfo
  , operatorsMap :: Map QVarOp OpInfo
  , aliasedModulesMap :: Map (Module /\ Module) (Set Module)
  , importedModulesMap :: Map Module (Set Module)
  | r
  }

reloadModule
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => String
  -> m (Either (Array ParseError) Unit)
reloadModule moduleText =
  traverse (\x -> unRegisterModuleDef x *> registerModuleDef x)
    $ runParser moduleDef moduleText

unRegisterModuleDef
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => ModuleDef
  -> m Unit
unRegisterModuleDef (ModuleDef module' _ _ _) =
  modify_ \st -> st
    { fnsMap = Map.deleteWhen (\(QVar x _) -> x == Just module')
        st.fnsMap
    , operatorsMap = Map.deleteWhen (\(QVarOp x _) -> x == Just module')
        st.operatorsMap
    , aliasedModulesMap = Map.deleteWhen (eq module' <<< fst)
        st.aliasedModulesMap
    , importedModulesMap = Map.delete module' st.importedModulesMap
    }

registerModuleDef
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => ModuleDef
  -> m Unit
registerModuleDef (ModuleDef module' imports opDefs fnDefs) =
  registerModuleImports module' imports
    *> registerModuleOps module' opDefs
    *>
      registerModuleFns module' fnDefs

registerModuleImports
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => Module
  -> Array ModuleImport
  -> m Unit
registerModuleImports module' imports =
  traverse_ registerImport imports
  where
  registerImport (ModuleImport mod (Just alias)) =
    modify_ \st -> st
      { aliasedModulesMap = Map.insertWith Set.union (module' /\ alias)
          (Set.singleton mod)
          st.aliasedModulesMap
      }
  registerImport (ModuleImport mod Nothing) =
    modify_ \st -> st
      { importedModulesMap = Map.insertWith Set.union module'
          (Set.singleton mod)
          st.importedModulesMap
      }

registerModuleOps
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => Module
  -> Array OpDef
  -> m Unit
registerModuleOps opModule opDefs =
  modify_ \st -> st
    { operatorsMap = Map.union (Map.fromFoldable (toEntry <$> opDefs))
        st.operatorsMap
    }
  where
  toEntry (OpDef opName fnName associativity precedence) =
    QVarOp (Just opModule) opName /\
      { id: { opModule, opName }
      , fnName: QVar (Just opModule) fnName
      , precedence
      , associativity
      }

registerModuleFns
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => Module
  -> Array FnDef
  -> m Unit
registerModuleFns fnModule fnDefs =
  modify_ \st -> st
    { fnsMap = Map.union (Map.fromFoldable (toEntry <$> fnDefs))
        st.fnsMap
    }
  where
  toEntry (FnDef fnName params body) =
    QVar (Just fnModule) fnName /\
      FnInfo
        { id: Just { fnModule, fnName }
        , params
        , body
        , scope: zero
        , argsMap: Map.empty
        }

