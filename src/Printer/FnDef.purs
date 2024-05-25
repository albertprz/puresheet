module App.Printer.FnDef where

import FatPrelude hiding (guard)
import Prim hiding (Type)

import App.Printer.Common (cell, cellValue, encloseArgs, encloseContext, encloseList, encloseParens, encloseSquare, encloseTuple, qVar, qVarOp, surroundDoc, var, varOp, zipWithInfixs, (<///>), (<//>))
import App.Printer.Pattern (pattern')
import App.Printer.Type (type')
import App.SyntaxTree.Common (Var)
import App.SyntaxTree.FnDef (CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), OpDef(..), PatternGuard(..))
import App.SyntaxTree.Type (Type)
import Data.Array as Array
import Data.String (null) as String
import Data.String.Utils (lines) as String
import Dodo (Doc, alignCurrentColumn, break, flexGroup, foldWithSeparator, text, (<+>))

opDef :: forall a. OpDef -> Doc a
opDef = case _ of
  OpDef x y z t -> varOp x
    <+> text "="
    <+> qVar y
    <+> text (show z <> show t)

fnDef :: forall a. Doc a -> FnDef -> Doc a
fnDef sep = case _ of
  FnDef x ys z t v -> var x
    <+> fnSignature ys z
    <+> text "="
    <+> fnDocs t
    <> sep
    <> fnBody v

fnSignature :: forall a. Array (Var /\ Maybe Type) -> Maybe Type -> Doc a
fnSignature params returnType =
  flexGroup (encloseArgs (map printParam params))
    <> fold ((text ":" <+> _) <<< type' <$> returnType)
  where
  printParam (x /\ y) = fold $ Array.cons (var x)
    (Array.fromFoldable ((text ":" <+> _) <<< type' <$> y))

fnDocs :: forall a. String -> Doc a
fnDocs docString = fold
  $ (((break <+> text "//") <+> _) <<< text)
  <$> filter (not <<< String.null) (String.lines docString)

fnBody :: forall a. FnBody -> Doc a
fnBody = flexGroup <<< case _ of
  FnApply x ys -> fnBody x <+> encloseArgs (map fnBody ys)
  Recur xs -> text "recur" <+> encloseArgs (map fnBody xs)
  LambdaFn xs y -> (encloseTuple (map var xs) <+> text "->") <//> fnBody y
  InfixFnApply xs ys -> zipWithInfixs qVarOp fnBody xs ys
  LeftOpSection x y -> text "_" <+> qVarOp x <+> fnBody y
  RightOpSection x y -> fnBody x <+> qVarOp y <+> text "_"
  WhereExpr x ys -> fnBody x <+> text "where"
    <+> encloseContext ((text "|" <+> _) <<< fnDef mempty <$> ys)
  CondExpr xs -> text "cond"
    <+> encloseContext (guardedFnBody (text "=>") <$> xs)
  SwitchExpr x ys -> text "switch" <+> encloseParens (fnBody x)
    <+> encloseContext ((text "|" <+> _) <<< caseBinding <$> ys)
  CellMatrixRange x y -> encloseSquare
    $ surroundDoc (text "||") (cell x <+> text ".." <+> cell y)
  CellArrayRange x y -> encloseSquare
    $ surroundDoc (text "|") (cell x <+> text ".." <+> cell y)
  ArrayRange x y -> encloseSquare
    (fnBody x <+> text ".." <+> fnBody y)
  Array' xs -> encloseList $ map fnBody xs
  FnVar x -> qVar x
  FnOp x -> qVarOp x
  Cell' x -> cell x
  CellValue' x -> cellValue x
  Object' _ -> mempty

caseBinding :: forall a. CaseBinding -> Doc a
caseBinding = case _ of
  CaseBinding x y -> pattern' x <+> maybeGuardedFnBody (text "=>") y

maybeGuardedFnBody :: forall a. Doc a -> MaybeGuardedFnBody -> Doc a
maybeGuardedFnBody sep = case _ of
  Guarded xs -> alignCurrentColumn $ foldWithSeparator break
    $ map (guardedFnBody sep) xs
  Standard x -> flexGroup (sep <//> fnBody x)

guardedFnBody :: forall a. Doc a -> GuardedFnBody -> Doc a
guardedFnBody sep = flexGroup <<< case _ of
  GuardedFnBody x y -> (guard x <+> sep) <///> (fnBody y)

guard :: forall a. Guard -> Doc a
guard x = text "?" <+> case x of
  Guard xs -> alignCurrentColumn $ foldWithSeparator break
    $ map patternGuard xs
  Otherwise -> text "otherwise"

patternGuard :: forall a. PatternGuard -> Doc a
patternGuard = case _ of
  PatternGuard x y -> pattern' x <+> text "<-" <+> fnBody y
  SimpleGuard x -> fnBody x
