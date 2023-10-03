module App.SyntaxTree.ModuleDef where

import FatPrelude

import App.SyntaxTree.Common (Module)
import App.SyntaxTree.FnDef (FnDef)

data ModuleDef =
  ModuleDef Module (Array ModuleImport) (Array FnDef)

data ModuleImport =
  ModuleImport Module (Maybe Module)
