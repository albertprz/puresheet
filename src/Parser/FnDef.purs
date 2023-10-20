module App.Parser.FnDef (opDef, fnDef, fnBody, statements) where

import FatPrelude

import App.Components.Table.Cell (Column(..), Row(..), buildCell)
import App.Parser.Common (argListOf, cellValue, isToken, qVar, qVarOp, token, var, varOp)
import App.Parser.Pattern (pattern')
import App.SyntaxTree.FnDef (Associativity(..), CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), OpDef(..), PatternGuard(..))
import Bookhound.Parser (ParseError(..), Parser, errorParser, withError)
import Bookhound.ParserCombinators (is, someSepBy, within, (<|>), (|+))
import Bookhound.Parsers.Char (comma, quote, upper)
import Bookhound.Parsers.Collections (listOf)
import Bookhound.Parsers.Number (posInt, unsignedInt)
import Bookhound.Parsers.String (withinCurlyBrackets, withinParens, withinSquareBrackets)
import Control.Lazy (defer)
import Data.Array as Array

opDef :: Parser OpDef
opDef = defer \_ -> withError "Operator definition"
  $ OpDef
  <$> varOp
  <* isToken "="
  <*> var
  <*> (L <$ is 'L' <|> R <$ is 'R')
  <*>
    ( mandatory "precedence must be between 0 and 12"
        (toEnum <$> unsignedInt)
    )

fnDef :: Parser FnDef
fnDef = defer \_ -> withError "Function definition"
  $ FnDef
  <$> var
  <*> (argListOf var <|> pure [])
  <* isToken "="
  <*> fnBody

fnBody :: Parser FnBody
fnBody = whereExpr <|> token openForm
  where
  fnApply = defer \_ -> FnApply <$> (token fnForm) <*>
    (fold <$> ((|+) $ argListOf openForm))
  lambdaFn = defer \_ -> LambdaFn <$> (argListOf var <|> pure <$> var)
    <*> (isToken "->" *> fnBody)
  infixFnApply = defer \_ -> uncurry InfixFnApply <$> sepByOps qVarOp
    infixArgForm
  leftOpSection = defer \_ -> uncurry LeftOpSection <$>
    ((/\) <$> (isToken "_" *> qVarOp) <*> infixArgForm)
  rightOpSection = defer \_ -> uncurry RightOpSection <$>
    ((/\) <$> infixArgForm <*> (qVarOp <* isToken "_"))
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
  fnForm = defer \_ -> fnVar <|> fnOp
  singleForm = defer \_ -> fnApply
    <|> array
    <|> cellMatrixRange
    <|> cellArrayRange
    <|> arrayRange
    <|> CellValue'
    <$> cellValue
    <|> Cell'
    <$> cell
    <|> fnOp
    <|> fnVar
  complexForm = defer \_ -> lambdaFn <|> opSection <|> infixFnApply <|>
    complexInfixForm
  complexInfixForm = defer \_ ->
    condExpr
      <|> switchExpr
      <|> withinParens lambdaFn
      <|> withinParens opSection
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
guard = defer \_ -> isToken "?"
  *>
    ( Otherwise <$ isToken "otherwise"
        <|> Guard
        <$> someSepBy comma patternGuard
    )

patternGuard :: Parser PatternGuard
patternGuard = defer \_ -> PatternGuard <$> (pattern' <* isToken "<-")
  <*> fnBody
  <|> SimpleGuard
  <$> fnBody

statements :: forall a. String -> Parser a -> Parser (Array a)
statements sep parser =
  ((|+) (isToken sep *> parser))

withinContext :: forall a. String -> Parser a -> Parser (Array a)
withinContext sep = withinCurlyBrackets <<< statements sep

sepByOps :: forall a b. Parser a -> Parser b -> Parser (Array a /\ Array b)
sepByOps sep p = do
  x <- p
  y <- (|+) ((/\) <$> sep <*> p)
  pure $ ((fst <$> y) /\ Array.cons x (snd <$> y))

mandatory :: forall a. String -> Parser (Maybe a) -> Parser a
mandatory tag maybeP =
  fromMaybe (errorParser $ NoMatch tag) <<< (map pure) =<< maybeP
