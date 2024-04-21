module App.Parser.FnDef (opDef, fnDef, fnBody, statements) where

import FatPrelude hiding (guard)

import App.Components.Spreadsheet.Cell (cellParser)
import App.Parser.Common (argListOf, cellValue, isToken, qVar, qVarOp, token, var, varOp)
import App.Parser.Pattern (pattern')
import App.Parser.Type (type')
import App.SyntaxTree.FnDef (Associativity(..), CaseBinding(..), FnBody(..), FnDef(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), OpDef(..), PatternGuard(..))
import Bookhound.Parser (Parser, withError)
import Bookhound.ParserCombinators (is, isNot, maybeSurroundedBy, sepByOps, someSepBy, surroundedBy, (</\>), (|*), (|+), (|?), (||*))
import Bookhound.Parsers.Char (comma, quote)
import Bookhound.Parsers.Collections (listOf)
import Bookhound.Parsers.Number (unsignedInt)
import Bookhound.Parsers.String (betweenCurly, betweenParens, betweenSquare, spacesOrTabs)
import Control.Lazy (defer)
import Data.String as String

opDef :: Parser OpDef
opDef = defer \_ -> withError "Operator definition"
  $ OpDef
  <$> varOp
  <*> (isToken "=" *> qVar)
  <*> (L <$ is 'L' <|> R <$ is 'R')
  <*> mandatory (toEnum <$> unsignedInt)
  where
  mandatory = (=<<) (fromMaybe empty <<< map pure)

fnDef :: Parser FnDef
fnDef = defer \_ -> withError "Function definition"
  $ FnDef
  <$> var
  <*> (argListOf annotatedVar <|> pure [])
  <*> (annotation <* isToken "=")
  <*> map (String.joinWith "\n") comments
  <*> fnBody
  where
  annotatedVar = var </\> annotation
  annotation = (|?) (isToken ":" *> type')
  comment = ((is "//" *> ((|?) spacesOrTabs) *> (||*) (isNot '\n')))
  comments = (|*) (maybeSurroundedBy spacesOrTabs comment <* is '\n')

fnBody :: Parser FnBody
fnBody = whereExpr <|> topLevelExpr
  where

  fnApply = defer \_ -> FnApply
    <$> token fnForm
    <*> argListOf openForm

  lambdaFn = defer \_ -> LambdaFn
    <$> (argListOf var <|> pure <$> var)
    <*> (isToken "->" *> fnBody)

  infixFnApply = token $ defer \_ ->
    uncurry InfixFnApply <$> sepByOps qVarOp infixArgForm

  leftOpSection = defer \_ -> uncurry LeftOpSection <$>
    ((isToken "_" *> qVarOp) </\> infixArgForm)
  rightOpSection = defer \_ -> uncurry RightOpSection <$>
    (infixArgForm </\> (qVarOp <* isToken "_"))
  opSection = defer \_ -> leftOpSection <|> rightOpSection

  whereExpr = defer \_ -> WhereExpr
    <$> topLevelExpr
    <*> (isToken "where" *> betweenContext "|" fnDef)

  condExpr = defer \_ -> CondExpr <$>
    ( isToken "cond" *> betweenCurly
        ((|+) (guardedFnBody (isToken "=>")))
    )
  switchExpr = defer \_ -> SwitchExpr
    <$> (isToken "switch" *> betweenParens openForm)
    <*> betweenContext "|" caseBinding

  cellMatrixRange = defer \_ -> betweenSquare
    $ surroundedBy (isToken "||")
        (CellMatrixRange <$> (cell <* isToken "..") <*> cell)
  cellArrayRange = defer \_ -> betweenSquare
    $ surroundedBy (isToken "|")
        (CellArrayRange <$> (cell <* isToken "..") <*> cell)
  arrayRange = defer \_ -> betweenSquare
    (ArrayRange <$> (openForm <* isToken "..") <*> openForm)

  array = defer \_ -> Array' <$> (token (listOf openForm))
  fnOp = defer \_ -> FnOp <$> (token quote *> qVarOp)
  fnVar = defer \_ -> FnVar <$> qVar
  cell = cellParser

  topLevelExpr = switchExpr <|> condExpr <|> openForm
  infixArgForm = defer \_ -> complexInfixForm <|> singleForm
  openForm = defer \_ -> complexForm <|> singleForm

  fnForm = defer \_ -> fnVar <|> fnOp
  singleForm = defer \_ ->
    array
      <|> cellMatrixRange
      <|> cellArrayRange
      <|> arrayRange
      <|> (CellValue' <$> cellValue)
      <|> (Cell' <$> cell)
      <|> fnOp
      <|> fnApply
      <|> fnVar

  complexForm = defer \_ -> lambdaFn <|> opSection <|> infixFnApply
  complexInfixForm = defer \_ -> betweenParens complexForm

caseBinding :: Parser CaseBinding
caseBinding = defer \_ -> CaseBinding
  <$> pattern'
  <*> maybeGuardedFnBody (isToken "=>")

maybeGuardedFnBody :: forall a. Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep =
  Guarded <$> (|+) (guardedFnBody sep)
    <|> (Standard <$> (sep *> fnBody))

guardedFnBody :: forall a. Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*> fnBody

guard :: Parser Guard
guard = defer \_ -> isToken "?" *>
  ( Otherwise <$ isToken "otherwise"
      <|> (Guard <$> someSepBy comma patternGuard)
  )

patternGuard :: Parser PatternGuard
patternGuard = defer \_ ->
  (PatternGuard <$> (pattern' <* isToken "<-") <*> fnBody)
    <|> (SimpleGuard <$> fnBody)

statements :: forall a. String -> Parser a -> Parser (Array a)
statements sep parser = (|+) (isToken sep *> parser)

betweenContext :: forall a. String -> Parser a -> Parser (Array a)
betweenContext sep = betweenCurly <<< statements sep
