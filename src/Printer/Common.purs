module App.Printer.Common where

import FatPrelude

import App.Components.Spreadsheet.Cell (Cell, CellValue(..), showCell)
import App.SyntaxTree.Common (Module, QVar, QVarOp, Var, VarOp)
import Data.Array as Array
import Dodo (Doc, break, enclose, flexAlt, foldWithSeparator, indent, lines, paragraph, plainText, print, space, spaceBreak, text, twoSpaces, (<+>))

cell :: forall a. Cell -> Doc a
cell = text <<< showCell

cellValue :: forall a. CellValue -> Doc a
cellValue (StringVal x) = text $ show x
cellValue (CharVal x) = text $ show x
cellValue x = text $ show x

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
encloseList = encloseSquare <<< foldCsv

encloseArgs :: forall f a. Foldable f => f (Doc a) -> Doc a
encloseArgs = encloseParens <<< foldCsv

encloseContext :: forall f a. Foldable f => f (Doc a) -> Doc a
encloseContext =
  encloseCurly <<< surroundDoc break <<< indent <<< lines

encloseParens :: forall a. Doc a -> Doc a
encloseParens = enclose (text "(") (text ")")

encloseSquare :: forall a. Doc a -> Doc a
encloseSquare = enclose (text "[") (text "]")

encloseCurly :: forall a. Doc a -> Doc a
encloseCurly = enclose (text "{") (text "}")

foldCsv :: forall f a. Foldable f => f (Doc a) -> Doc a
foldCsv = foldWithSeparator (text "," <> spaceBreak)

surroundDoc :: forall a. Doc a -> Doc a -> Doc a
surroundDoc x = enclose x x

zipWithInfixs
  :: forall a b c. (a -> Doc c) -> (b -> Doc c) -> Array a -> Array b -> Doc c
zipWithInfixs infixFn valFn infixs vals =
  paragraph $ Array.cons (valFn $ unsafeFromJust $ Array.head vals)
    ( Array.zipWith (\x y -> infixFn x <+> valFn y) infixs
        (fold $ Array.tail vals)
    )

appendSpaceBreakIndent :: forall a. Doc a -> Doc a -> Doc a
appendSpaceBreakIndent x y =
  x <> flexAlt (space <> y) (break <> indent y)

infixl 2 appendSpaceBreakIndent as <//>

appendSpaceBreakIndent2 :: forall a. Doc a -> Doc a -> Doc a
appendSpaceBreakIndent2 x y =
  x <> flexAlt (space <> y) (break <> indent (indent y))

infixl 2 appendSpaceBreakIndent2 as <///>

printDefault :: forall a. Doc a -> String
printDefault =
  print plainText (twoSpaces { pageWidth = 90 })
