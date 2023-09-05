module App.Parsers.FnDef where

import FatPrelude

import App.Components.Table.Cell (Column(..), Row(..), buildCell)
import App.Parsers.Common (argListOf, cellValue, ctor, token, var, varOp)
import App.Parsers.Pattern (pattern')
import App.SyntaxTrees.FnDef (CaseBinding(..), FnBody(..), FnDef(..), FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
import Bookhound.FatPrelude (maybeToArray)
import Bookhound.Parser (Parser, withError)
import Bookhound.ParserCombinators (is, someSepBy, (<|>), (|*), (|+), (|?))
import Bookhound.Parsers.Char (comma, dot, quote, upper)
import Bookhound.Parsers.Collections (listOf, tupleOf)
import Bookhound.Parsers.Number (posInt)
import Bookhound.Parsers.String (withinCurlyBrackets, withinParens, withinSquareBrackets)
import Control.Lazy (defer)
import Data.Array as Array

fnDef :: Parser FnDef
fnDef = defer \_ -> withError "Function definition"
  $ FnDef
  <$> (tupleOf var <|> pure <$> var)
  <*> (|*) pattern'
  <*> maybeGuardedFnBody (is "=")

fnBody :: Parser FnBody
fnBody = whereExpr <|> openForm
  where
  fnApply = defer \_ -> FnApply <$> delimitedForm <*> argListOf delimitedForm
  infixFnApply = defer \_ -> uncurry InfixFnApply <$> sepByOps varOp
    infixArgForm
  leftOpSection = defer \_ -> uncurry LeftOpSection <$> withinParens
    ((/\) <$> varOp <*> openForm)
  rightOpSection = defer \_ -> uncurry RightOpSection <$> withinParens
    ((/\) <$> openForm <*> varOp)
  opSection = defer \_ -> leftOpSection <|> rightOpSection
  whereExpr = defer \_ -> Bindings <$> openForm
    <* is "where"
    <*> withinContext fnDef
  multiWayIfExpr = defer \_ -> MultiWayIfExpr <$>
    (is "if" *> (withinContext $ guardedFnBody $ is "->"))
  switchExpr = defer \_ -> SwitchExpr <$> (is "switch" *> withinParens openForm)
    <*>
      withinContext caseBinding
  listRange = defer \_ -> withinSquareBrackets $ ListRange
    <$> (openForm <* is "..")
    <*> openForm
  list = defer \_ -> List <$> listOf openForm
  fnOp = defer \_ -> FnOp <$> (quote *> varOp)
  fnVar = defer \_ ->
    FnVar'
      <$> (Selection <$> var <* dot <*> someSepBy dot var)
      <|> FnVar'
      <<< Var'
      <$> var
      <|> FnVar'
      <<< Ctor'
      <$> ctor
  cell = buildCell <$> (Column <$> upper) <*> (Row <$> posInt)
  recordCreate = defer \_ -> RecordCreate <$> ctor <*> recordFields
  recordUpdate = defer \_ -> RecordUpdate <$> delimitedForm <*> recordFields
  recordFields = defer \_ -> withinCurlyBrackets (someSepBy comma recordField)
  recordField = defer \_ -> (/\) <$> var <*> (is "=" *> openForm)
  infixArgForm = defer \_ -> complexInfixForm <|> withinParens complexInfixForm
    <|>
      singleForm
  openForm = defer \_ -> complexForm <|> singleForm <|> withinParens
    (complexForm <|> singleForm)
  delimitedForm = defer \_ -> singleForm <|> withinParens complexForm <|>
    withinParens
      singleForm
  singleForm = defer \_ -> fnApply <|> fnOp <|> fnVar <|> CellValue'
    <$> cellValue
    <|> Cell'
    <$> cell
    <|> listRange
    <|> list
    <|>
      opSection
  complexForm = defer \_ -> infixFnApply <|> complexInfixForm
  complexInfixForm = defer \_ ->
    multiWayIfExpr
      <|> switchExpr
      <|> withinParens infixFnApply
      <|> recordCreate
      <|> recordUpdate

caseBinding :: Parser CaseBinding
caseBinding = defer \_ -> CaseBinding <$> pattern' <*> maybeGuardedFnBody
  (is "->")

maybeGuardedFnBody :: forall a. Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep = Guarded <$> (|+) (guardedFnBody sep)
  <|> Standard
  <$>
    (sep *> fnBody)

guardedFnBody :: forall a. Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*>
  fnBody

guard :: Parser Guard
guard = defer \_ -> Otherwise <$ (is "?" *> token (is "otherwise")) <|> Guard
  <$>
    (is "?" *> someSepBy comma patternGuard)

patternGuard :: Parser PatternGuard
patternGuard = defer \_ -> PatternGuard <$> (pattern' <* is "<-") <*> fnBody
  <|> SimpleGuard
  <$> fnBody

statements :: forall a. Parser a -> Parser (Array a)
statements parser = defer \_ -> fold <$> someSepBy (is "|")
  (maybeToArray <$> (|?) parser)

withinContext :: forall a. Parser a -> Parser (Array a)
withinContext = withinCurlyBrackets <<< statements

sepByOps :: forall a b. Parser a -> Parser b -> Parser (Array a /\ Array b)
sepByOps sep p = do
  x <- p
  y <- (|+) ((/\) <$> sep <*> p)
  pure $ ((fst <$> y) /\ Array.cons x (snd <$> y))
