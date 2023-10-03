module App.Interpreter.Module (loadModule) where

import FatPrelude

import App.Parser.ModuleDef (moduleDef)
import App.SyntaxTree.Common (Module, QVar(..))
import App.SyntaxTree.FnDef (FnDef(..), FnInfo)
import App.SyntaxTree.ModuleDef (ModuleDef(..), ModuleImport(..))
import Bookhound.Parser (ParseError, runParser)
import Data.Map as Map
import Data.Set as Set

type RegisterModuleCtx r =
  { fnsMap :: Map QVar FnInfo
  , aliasedModulesMap :: Map (Module /\ Module) (Set Module)
  , importedModulesMap :: Map Module (Set Module)
  | r
  }

loadModule
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => String
  -> m (Either (Array ParseError) Unit)
loadModule moduleText =
  traverse (\x -> registerModuleDef x) $ runParser moduleDef moduleText

registerModuleDef
  :: forall r m
   . ModuleDef
  -> MonadState (RegisterModuleCtx r) m
  => m Unit
registerModuleDef (ModuleDef module' imports fnDefs) =
  registerModuleImports module' imports *>
    registerModuleFns module' fnDefs

registerModuleImports
  :: forall r m
   . Module
  -> Array ModuleImport
  -> MonadState (RegisterModuleCtx r) m
  => m Unit
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

registerModuleFns
  :: forall r m
   . Module
  -> Array FnDef
  -> MonadState (RegisterModuleCtx r) m
  => m Unit
registerModuleFns fnModule fnDefs =
  modify_ \st -> st
    { fnsMap = Map.union (Map.fromFoldable (toEntry <$> fnDefs))
        st.fnsMap
    }
  where
  toEntry (FnDef fnName params body) =
    QVar (Just fnModule) fnName /\
      { id: Just { fnModule, fnName }, params, body, scope: zero }
