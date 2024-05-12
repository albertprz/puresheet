module App.Printer.FnDef where

import FatPrelude hiding (guard)

import App.Printer.Common (cell, cellValue, encloseArgs, encloseContext, encloseList, encloseParens, encloseSquare, encloseTuple, qVar, qVarOp, surroundDoc, var, varOp, zipWithInfixs)
import App.Printer.Pattern (pattern')
import App.Printer.Type (type')
import App.SyntaxTree.FnDef (CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), OpDef(..), PatternGuard(..))
import Data.Array as Array
import Data.String.Utils as String
import Dodo (Doc, break, foldWithSeparator, text, (<%>), (<+>), (</>))

opDef :: forall a. OpDef -> Doc a
opDef = case _ of
  OpDef x y z t -> varOp x
    <+> text "="
    <+> qVar y
    <+> text (show z <> show t)

fnDef :: forall a. FnDef -> Doc a
fnDef = case _ of
  FnDef x ys z t v -> var x
    <+> encloseArgs (map printParam ys)
    <+> fold ((text ":" <+> _) <<< type' <$> z)
    <+> text "="
    <+> fold (((break <+> text "//") <+> _) <<< text <$> String.lines t)
    <%> fnBody v
  where
  printParam (x /\ y) = fold $ Array.cons (var x)
    (Array.fromFoldable ((text ":" <+> _) <<< type' <$> y))

fnBody :: forall a. FnBody -> Doc a
fnBody = case _ of
  FnApply x ys -> fnBody x </> encloseArgs (map fnBody ys)
  Recur xs -> text "recur" </> encloseArgs (map fnBody xs)
  LambdaFn xs y -> (encloseTuple (map var xs) <+> text "->") </> fnBody y
  InfixFnApply xs ys -> zipWithInfixs qVarOp fnBody xs ys
  LeftOpSection x y -> text "_" <+> qVarOp x <+> fnBody y
  RightOpSection x y -> fnBody x <+> qVarOp y <+> text "_"
  WhereExpr x ys -> fnBody x <+> text "where"
    <+> encloseContext ((text "|" <+> _) <<< fnDef <$> ys)
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
  CaseBinding x y -> pattern' x </> maybeGuardedFnBody (text "=>") y

maybeGuardedFnBody :: forall a. Doc a -> MaybeGuardedFnBody -> Doc a
maybeGuardedFnBody sep = case _ of
  Guarded xs -> foldWithSeparator break $ map (guardedFnBody sep) xs
  Standard x -> sep <+> fnBody x

guardedFnBody :: forall a. Doc a -> GuardedFnBody -> Doc a
guardedFnBody sep = case _ of
  GuardedFnBody x y -> guard x </> (sep <+> fnBody y)

guard :: forall a. Guard -> Doc a
guard x = text "?" <+> case x of
  Guard xs -> foldWithSeparator break $ map patternGuard xs
  Otherwise -> text "otherwise"

patternGuard :: forall a. PatternGuard -> Doc a
patternGuard = case _ of
  PatternGuard x y -> pattern' x <+> text "<-" <+> fnBody y
  SimpleGuard x -> fnBody x
