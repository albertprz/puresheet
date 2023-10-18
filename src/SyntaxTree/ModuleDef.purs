module App.SyntaxTree.ModuleDef where

import FatPrelude

import App.SyntaxTree.Common (Module)
import App.SyntaxTree.FnDef (FnDef, OpDef)

data ModuleDef =
  ModuleDef Module (Array ModuleImport) (Array OpDef) (Array FnDef)

data ModuleImport =
  ModuleImport Module (Maybe Module)

