module App.Parsers.FnDef (fnDef, fnBody) where

import FatPrelude

import App.Components.Table.Cell (Column(..), Row(..), buildCell)
import App.Parsers.Common (argListOf, cellValue, isToken, token, var, varOp)
import App.Parsers.Pattern (pattern')
import App.SyntaxTrees.FnDef (CaseBinding(..), FnBody(..), FnDef(..), FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
import Bookhound.Parser (Parser, withError)
import Bookhound.ParserCombinators (someSepBy, (<|>), (|*), (|+), (|?))
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
  <*> (|*) var
  <*> fnBody

fnBody :: Parser FnBody
fnBody = whereExpr <|> openForm
  where
  fnApply = defer \_ -> FnApply <$> fnForm <*> argListOf openForm
  infixFnApply = defer \_ -> uncurry InfixFnApply <$> sepByOps varOp
    infixArgForm
  leftOpSection = defer \_ -> uncurry LeftOpSection <$> withinParens
    ((/\) <$> varOp <*> openForm)
  rightOpSection = defer \_ -> uncurry RightOpSection <$> withinParens
    ((/\) <$> openForm <*> varOp)
  opSection = defer \_ -> leftOpSection <|> rightOpSection
  whereExpr = defer \_ -> WhereExpr <$> openForm
    <* isToken "where"
    <*> withinContext fnDef
  condExpr = defer \_ -> CondExpr <$>
    (isToken "cond" *> (withinContext $ guardedFnBody $ isToken "->"))
  switchExpr = defer \_ -> SwitchExpr
    <$> (isToken "switch" *> withinParens openForm)
    <*>
      withinContext caseBinding
  listRange = defer \_ -> withinSquareBrackets $ ListRange
    <$> (openForm <* isToken "..")
    <*> openForm
  matrixRange = defer \_ -> withinSquareBrackets $ withinSquareBrackets
    $ MatrixRange
    <$> (cell <* isToken "..")
    <*> cell
  list = defer \_ -> List <$> listOf openForm
  fnOp = defer \_ -> FnOp <$> (quote *> varOp)
  fnVar = defer \_ ->
    FnVar' <<< Var' <$> var
  -- <|> FnVar' <<< Ctor' <$> ctor
  cell = buildCell <$> (Column <$> upper) <*> (Row <$> posInt)
  infixArgForm = defer \_ ->
    complexInfixForm
      <|> withinParens complexInfixForm
      <|> singleForm
  openForm = defer \_ -> complexForm <|> singleForm
    <|> withinParens (complexForm <|> singleForm)
  fnForm = defer \_ -> fnVar <|> fnOp <|> withinParens (fnApply <|> complexForm)
  singleForm = defer \_ -> fnApply
    <|> matrixRange
    <|> listRange
    <|> list
    <|> opSection
    <|> Cell'
    <$> cell
    <|> fnOp
    <|> fnVar
    <|> CellValue'
    <$> cellValue
  complexForm = defer \_ -> infixFnApply <|> complexInfixForm
  complexInfixForm = defer \_ ->
    condExpr
      <|> switchExpr
      <|> withinParens infixFnApply

caseBinding :: Parser CaseBinding
caseBinding = defer \_ -> CaseBinding <$> pattern' <*> maybeGuardedFnBody
  (isToken "->")

maybeGuardedFnBody :: forall a. Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep = Guarded <$> (|+) (guardedFnBody sep)
  <|> Standard
  <$>
    (sep *> fnBody)

guardedFnBody :: forall a. Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*>
  fnBody

guard :: Parser Guard
guard = defer \_ -> Otherwise <$ (isToken "?" *> token (isToken "otherwise"))
  <|> Guard
  <$>
    (isToken "?" *> someSepBy comma patternGuard)

patternGuard :: Parser PatternGuard
patternGuard = defer \_ -> PatternGuard <$> (pattern' <* isToken "<-")
  <*> fnBody
  <|> SimpleGuard
  <$> fnBody

statements :: forall a. Parser a -> Parser (Array a)
statements parser = defer \_ -> fold <$> someSepBy (isToken "|")
  (maybeToArray <$> (|?) parser)

withinContext :: forall a. Parser a -> Parser (Array a)
withinContext = withinCurlyBrackets <<< statements

sepByOps :: forall a b. Parser a -> Parser b -> Parser (Array a /\ Array b)
sepByOps sep p = do
  x <- p
  y <- (|+) ((/\) <$> sep <*> p)
  pure $ ((fst <$> y) /\ Array.cons x (snd <$> y))
