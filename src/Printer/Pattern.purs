module App.Printer.Pattern where

import FatPrelude

import App.Printer.Common (cellValue, encloseList, var)
import App.SyntaxTree.Pattern (Pattern(..))
import Dodo (Doc, flexGroup, text, (<+>))

pattern' :: forall a. Pattern -> Doc a
pattern' = flexGroup <<< case _ of
  VarPattern x -> var x
  LitPattern x -> cellValue x
  AliasedPattern x y -> var x <+> text "@" <+> pattern' y
  ArrayPattern x -> encloseList $ map pattern' x
  Wildcard -> text "_"
  Spread -> text "..."
