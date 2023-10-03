module App.Parser.FnDef (fnDef, fnBody, statements) where

import FatPrelude

import App.Components.Table.Cell (Column(..), Row(..), buildCell)
import App.Parser.Common (argListOf, cellValue, isToken, qVar, qVarOp, token, var)
import App.Parser.Pattern (pattern')
import App.SyntaxTree.FnDef (CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
import Bookhound.Parser (Parser, withError)
import Bookhound.ParserCombinators (someSepBy, within, (<|>), (|+), (|?))
import Bookhound.Parsers.Char (comma, quote, upper)
import Bookhound.Parsers.Collections (listOf)
import Bookhound.Parsers.Number (posInt)
import Bookhound.Parsers.String (withinCurlyBrackets, withinParens, withinSquareBrackets)
import Control.Lazy (defer)
import Data.Array as Array

fnDef :: Parser FnDef
fnDef = defer \_ -> withError "Function definition"
  $ FnDef
  <$> var
  <*> (argListOf var <|> pure [])
  <* isToken "="
  <*> fnBody

fnBody :: Parser FnBody
fnBody = whereExpr <|> openForm
  where
  fnApply = defer \_ -> FnApply <$> token fnForm <*> argListOf openForm
  infixFnApply = defer \_ -> uncurry InfixFnApply <$> sepByOps qVarOp
    infixArgForm
  leftOpSection = defer \_ -> uncurry LeftOpSection <$> withinParens
    ((/\) <$> (isToken "_" *> qVarOp) <*> openForm)
  rightOpSection = defer \_ -> uncurry RightOpSection <$> withinParens
    ((/\) <$> openForm <*> (qVarOp <* isToken "_"))
  opSection = defer \_ -> leftOpSection <|> rightOpSection
  whereExpr = defer \_ -> WhereExpr <$> openForm
    <* isToken "where"
    <*> withinContext "|" fnDef
  condExpr = defer \_ -> CondExpr <$>
    ( isToken "cond" *> withinCurlyBrackets
        ((|+) (guardedFnBody (isToken "=>")))
    )
  switchExpr = defer \_ -> SwitchExpr
    <$> (isToken "switch" *> withinParens openForm)
    <*>
      withinContext "|" caseBinding
  cellMatrixRange = defer \_ -> withinSquareBrackets $ within (isToken "||")
    $ CellMatrixRange
    <$> (cell <* isToken "..")
    <*> cell
  cellArrayRange = defer \_ -> withinSquareBrackets $ within (isToken "|")
    $ CellArrayRange
    <$> (cell <* isToken "..")
    <*> cell
  arrayRange = defer \_ -> withinSquareBrackets $ ArrayRange
    <$> (openForm <* isToken "..")
    <*> openForm
  array = defer \_ -> Array' <$> (token (listOf openForm))
  fnOp = defer \_ -> FnOp <$> (token quote *> qVarOp)

  fnVar = defer \_ -> FnVar <$> qVar
  cell = buildCell <$> (Column <$> upper) <*> (Row <$> posInt)
  infixArgForm = defer \_ ->
    complexInfixForm
      <|> withinParens complexInfixForm
      <|> singleForm
  openForm = defer \_ -> complexForm <|> singleForm
    <|> withinParens (complexForm <|> singleForm)
  fnForm = defer \_ -> fnVar <|> fnOp <|> withinParens (fnApply <|> complexForm)
  singleForm = defer \_ -> fnApply
    <|> array
    <|> cellMatrixRange
    <|> cellArrayRange
    <|> arrayRange
    <|> opSection
    <|> CellValue'
    <$> cellValue
    <|> Cell'
    <$> cell
    <|> fnOp
    <|> fnVar
  complexForm = defer \_ -> infixFnApply <|> complexInfixForm
  complexInfixForm = defer \_ ->
    condExpr
      <|> switchExpr
      <|> withinParens infixFnApply

caseBinding :: Parser CaseBinding
caseBinding = defer \_ -> CaseBinding <$> pattern' <*> maybeGuardedFnBody
  (isToken "=>")

maybeGuardedFnBody :: forall a. Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep = Guarded <$> (|+) (guardedFnBody sep)
  <|> Standard
  <$>
    (sep *> fnBody)

guardedFnBody :: forall a. Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*>
  fnBody

guard :: Parser Guard
guard = defer \_ -> Otherwise <$ (isToken "?" *> isToken "otherwise")
  <|> Guard
  <$>
    (isToken "?" *> someSepBy comma patternGuard)

patternGuard :: Parser PatternGuard
patternGuard = defer \_ -> PatternGuard <$> (pattern' <* isToken "<-")
  <*> fnBody
  <|> SimpleGuard
  <$> fnBody

statements :: forall a. String -> Parser a -> Parser (Array a)
statements sep parser = defer \_ -> fold <$> someSepBy (isToken sep)
  (maybeToArray <$> (|?) parser)

withinContext :: forall a. String -> Parser a -> Parser (Array a)
withinContext sep = withinCurlyBrackets <<< statements sep

sepByOps :: forall a b. Parser a -> Parser b -> Parser (Array a /\ Array b)
sepByOps sep p = do
  x <- p
  y <- (|+) ((/\) <$> sep <*> p)
  pure $ ((fst <$> y) /\ Array.cons x (snd <$> y))
