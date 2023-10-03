module App.Parser.ModuleDef where

import FatPrelude

import App.Parser.Common (module')
import App.Parser.FnDef (fnDef, statements)
import App.SyntaxTree.ModuleDef (ModuleDef(..), ModuleImport(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (is, (|?))

moduleDef :: Parser ModuleDef
moduleDef = ModuleDef
  <$> (is "module" *> module')
  <*> statements "import" moduleImport
  <*> statements "def" fnDef

moduleImport :: Parser ModuleImport
moduleImport = ModuleImport
  <$> module'
  <*> ((|?) (is "as" *> module'))
