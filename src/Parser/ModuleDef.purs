module App.Parser.ModuleDef where

import FatPrelude

import App.Parser.Common (module')
import App.Parser.FnDef (fnDef, opDef, statements)
import App.SyntaxTree.ModuleDef (ModuleDef(..), ModuleImport(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (is, (|?))

moduleDef :: Parser ModuleDef
moduleDef = ModuleDef
  <$> (is "module" *> module')
  <*> (fold <$> (|?) (statements "import" moduleImport))
  <*> (fold <$> (|?) (statements "op" opDef))
  <*> (fold <$> (|?) (statements "def" fnDef))

moduleImport :: Parser ModuleImport
moduleImport = ModuleImport
  <$> module'
  <*> ((|?) (is "as" *> module'))
