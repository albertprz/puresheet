module App.Interpreter.Module (reloadModule) where

import FatPrelude

import App.Parser.ModuleDef (moduleDef)
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..))
import App.SyntaxTree.FnDef (FnDef(..), FnInfo(..), OpDef(..), OpInfo)
import App.SyntaxTree.ModuleDef (ModuleDef(..), ModuleImport(..))
import App.Utils.HashMap (deleteWhen) as HashMap
import Bookhound.Parser (ParseError, runParser)
import Data.HashMap (delete, empty, fromArray, insertWith, union) as HashMap
import Data.Set as Set

type RegisterModuleCtx r =
  { fnsMap :: HashMap QVar FnInfo
  , operatorsMap :: HashMap QVarOp OpInfo
  , aliasedModulesMap :: HashMap (Module /\ Module) (Set Module)
  , importedModulesMap :: HashMap Module (Set Module)
  , modules :: Set Module
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
    { fnsMap = HashMap.deleteWhen (\(QVar x _) -> x == Just module')
        st.fnsMap
    , operatorsMap = HashMap.deleteWhen (\(QVarOp x _) -> x == Just module')
        st.operatorsMap
    , aliasedModulesMap = HashMap.deleteWhen (eq module' <<< fst)
        st.aliasedModulesMap
    , importedModulesMap = HashMap.delete module' st.importedModulesMap
    }

registerModuleDef
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => ModuleDef
  -> m Unit
registerModuleDef (ModuleDef module' imports opDefs fnDefs) =
  registerModule module'
    *> registerModuleImports module' imports
    *> registerModuleOps module' opDefs
    *> registerModuleFns module' fnDefs

registerModule
  :: forall r m
   . MonadState (RegisterModuleCtx r) m
  => Module
  -> m Unit
registerModule module' =
  modify_ \st -> st
    { modules = Set.insert module' st.modules
    }

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
      { aliasedModulesMap = HashMap.insertWith Set.union (module' /\ alias)
          (Set.singleton mod)
          st.aliasedModulesMap
      }
  registerImport (ModuleImport mod Nothing) =
    modify_ \st -> st
      { importedModulesMap = HashMap.insertWith Set.union module'
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
    { operatorsMap = HashMap.union (HashMap.fromArray $ map toEntry opDefs)
        st.operatorsMap
    }
  where
  toEntry (OpDef opName (QVar fnModule fnName) associativity precedence) =
    QVarOp (Just opModule) opName /\
      { id: { opModule, opName }
      , fnName: QVar (fnModule <|> Just opModule) fnName
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
    { fnsMap = HashMap.union
        (HashMap.fromArray $ map toEntry fnDefs)
        st.fnsMap
    }
  where
  toEntry (FnDef fnName params returnType doc body) =
    QVar (Just fnModule) fnName /\
      FnInfo
        { id: Just { fnModule, fnName }
        , params
        , body
        , scope: zero
        , argsMap: HashMap.empty
        , returnType
        , doc
        }
