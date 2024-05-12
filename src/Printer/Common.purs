module App.Printer.Common where

import FatPrelude

import App.Components.Spreadsheet.Cell (Cell, CellValue)
import App.SyntaxTree.Common (Module, QVar, QVarOp, Var, VarOp)
import Data.Array as Array
import Dodo (Doc, break, enclose, foldWithSeparator, indent, lines, text, (<+>))

cell :: forall a. Cell -> Doc a
cell = text <<< show

cellValue :: forall a. CellValue -> Doc a
cellValue = text <<< show

var :: forall a. Var -> Doc a
var = text <<< show

varOp :: forall a. VarOp -> Doc a
varOp = text <<< show

module' :: forall a. Module -> Doc a
module' = text <<< show

qVar :: forall a. QVar -> Doc a
qVar = text <<< show

qVarOp :: forall a. QVarOp -> Doc a
qVarOp = text <<< show

encloseTuple :: forall a. Array (Doc a) -> Doc a
encloseTuple xs
  | length xs > 1 = encloseArgs xs
  | otherwise = unsafeFromJust $ Array.head xs

encloseList :: forall f a. Foldable f => f (Doc a) -> Doc a
encloseList = encloseSquare <<< foldWithSeparator (text ",")

encloseArgs :: forall f a. Foldable f => f (Doc a) -> Doc a
encloseArgs = encloseParens <<< foldWithSeparator (text ",")

encloseContext :: forall f a. Foldable f => f (Doc a) -> Doc a
encloseContext =
  encloseCurly <<< surroundDoc break <<< indent <<< lines

encloseParens :: forall a. Doc a -> Doc a
encloseParens = enclose (text "(") (text ")")

encloseSquare :: forall a. Doc a -> Doc a
encloseSquare = enclose (text "[") (text "]")

encloseCurly :: forall a. Doc a -> Doc a
encloseCurly = enclose (text "{") (text "}")

surroundDoc :: forall a. Doc a -> Doc a -> Doc a
surroundDoc x = enclose x x

zipWithInfixs
  :: forall a b c. (a -> Doc c) -> (b -> Doc c) -> Array a -> Array b -> Doc c
zipWithInfixs infixFn valFn infixs vals =
  fold $ Array.cons (valFn $ unsafeFromJust $ Array.head vals)
    ( Array.zipWith (\x y -> infixFn x <+> valFn y) infixs
        (fold $ Array.tail vals)
    )
