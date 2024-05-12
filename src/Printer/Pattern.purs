module App.Printer.Pattern where

import FatPrelude

import App.Printer.Common (cellValue, var)
import App.SyntaxTree.Pattern (Pattern(..))
import Dodo (Doc, text, words, (<+>))

pattern' :: forall a. Pattern -> Doc a
pattern' = case _ of
  VarPattern x -> var x
  LitPattern x -> cellValue x
  AliasedPattern x y -> var x <+> text "@" <+> pattern' y
  ArrayPattern x -> words $ map pattern' x
  Wildcard -> text "_"
  Spread -> text "..."
