module App.Printer.ModuleDef where

import FatPrelude hiding (guard)

import App.Printer.Common (surroundDoc)
import App.Printer.FnDef (fnDef, opDef)
import App.SyntaxTree.ModuleDef (ModuleDef(..), ModuleImport(..))
import Dodo (Doc, break, foldWithSeparator, text, (<%>), (<+>))

moduleDef :: forall a. ModuleDef -> Doc a
moduleDef = case _ of
  ModuleDef x y z t -> text "module"
    <+> text (show x)
    <%> statements "import" moduleImport break y
    <%> statements "op" opDef break z
    <%> statements "def" fnDef break t

moduleImport :: forall a. ModuleImport -> Doc a
moduleImport = case _ of
  ModuleImport x y -> text (show x)
    <+> fold ((text "as" <+> _) <<< text <<< show <$> y)

statements :: forall a b. String -> (a -> Doc b) -> Doc b -> Array a -> Doc b
statements term fn sep =
  surroundDoc break <<< foldWithSeparator sep <<< map ((text term <+> _) <<< fn)
