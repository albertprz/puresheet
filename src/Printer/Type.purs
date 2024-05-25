module App.Printer.Type where

import FatPrelude
import Prim hiding (Type)

import App.Printer.Common (surroundDoc)
import App.SyntaxTree.Type (Type(..), TypeParam, TypeVar)
import Dodo (Doc, enclose, encloseWithSeparator, foldWithSeparator, space, text, (<+>))

type' :: forall a. Type -> Doc a
type' = case _ of
  TypeApply x ys -> type' x <+> encloseTypeArgs (type' <$> ys)
  ArrowTypeApply xs ->
    foldWithSeparator (surroundDoc space $ text "->") (type' <$> xs)
  ArrayTypeApply x -> encloseType $ type' x
  TypeVar' x -> typeVar x
  TypeParam' x -> typeParam x

typeParam :: forall a. TypeParam -> Doc a
typeParam = text <<< show

typeVar :: forall a. TypeVar -> Doc a
typeVar = text <<< show

encloseTypeArgs :: forall f a. Foldable f => f (Doc a) -> Doc a
encloseTypeArgs = encloseWithSeparator (text "[") (text "]") (text ",")

encloseType :: forall a. Doc a -> Doc a
encloseType = enclose (text "[") (text "]")
