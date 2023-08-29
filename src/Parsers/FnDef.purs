module App.Parsers.FnDef where

import FatPrelude

import App.Parsers.Common (literal, nonTokenQVar, qCtor, qCtorOp, qVar, qVarOp, token, var)
import App.Parsers.Pattern (pattern')
import App.SyntaxTrees.FnDef (CaseBinding(..), DoStep(..), FnBody(..), FnDef(..), FnOp(..), FnVar(..), Guard(..), GuardedFnBody(..), MaybeGuardedFnBody(..), PatternGuard(..))
import Bookhound.FatPrelude (maybeToArray)
import Bookhound.Parser (Parser, withError)
import Bookhound.ParserCombinators (is, someSepBy, (<|>), (|*), (|+), (|?))
import Bookhound.Parsers.Char (closeParens, comma, dot, openParens)
import Bookhound.Parsers.Collections (collOf, listOf, tupleOf)
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
  fnApply = defer \_ -> FnApply <$> delimitedForm <*> csvOf openForm
  infixFnApply = defer \_ -> uncurry InfixFnApply <$> sepByOps fnOp infixArgForm
  leftOpSection = defer \_ -> uncurry LeftOpSection <$> withinParens
    ((/\) <$> fnOp <*> openForm)
  rightOpSection = defer \_ -> uncurry RightOpSection <$> withinParens
    ((/\) <$> openForm <*> fnOp)
  opSection = defer \_ -> leftOpSection <|> rightOpSection
  whereExpr = defer \_ -> Bindings <$> openForm
    <* is "where"
    <*> withinContext fnDef
  multiWayIfExpr = defer \_ -> MultiWayIfExpr <$>
    (is "if" *> (withinContext $ guardedFnBody $ is "->"))
  doExpr = defer \_ -> DoExpr <$> (is "do" *> withinContext doStep)
  caseOfExpr = defer \_ -> SwitchExpr <$> (is "switch" *> withinParens openForm)
    <*>
      withinContext caseBinding
  listRange = defer \_ -> withinSquareBrackets $ ListRange
    <$> (openForm <* is "..")
    <*> (|?) openForm
  list = defer \_ -> List <$> listOf openForm
  fnOp = defer \_ -> CtorOp' <$> qCtorOp <|> VarOp' <$> qVarOp
  fnOp' = defer \_ -> FnOp' <$> withinParens fnOp
  fnVar = defer \_ -> FnVar' <<< Selector <$> withinParens (dot *> var)
    <|> FnVar'
    <$> (Selection <$> nonTokenQVar <* dot <*> someSepBy dot var)
    <|> FnVar'
    <<< Var'
    <$> qVar
    <|> FnVar'
    <<< Ctor'
    <$> qCtor
  literal' = Literal' <$> literal
  recordCreate = defer \_ -> RecordCreate <$> qCtor <*> recordFields
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
  singleForm = defer \_ -> fnOp' <|> fnVar <|> literal' <|> listRange
    <|> list
    <|>
      opSection
  complexForm = defer \_ -> infixFnApply <|> complexInfixForm
  complexInfixForm = defer \_ -> fnApply
    <|> multiWayIfExpr
    <|> doExpr
    <|> caseOfExpr
    <|> withinParens infixFnApply
    <|> recordCreate
    <|> recordUpdate

doStep :: Parser DoStep
doStep = defer \_ -> DoBinding <$> (tupleOf var <|> pure <$> var) <* is "<-"
  <*> fnBody
  <|> LetBinding
  <$> fnDef
  <|> Body
  <$> fnBody

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
guard = defer \_ -> Otherwise <$ (is "|" *> token (is "otherwise")) <|> Guard
  <$>
    (is "|" *> someSepBy comma patternGuard)

patternGuard :: Parser PatternGuard
patternGuard = defer \_ -> PatternGuard <$> (pattern' <* is "<-") <*> fnBody
  <|> SimpleGuard
  <$> fnBody

statements :: forall a. Parser a -> Parser (Array a)
statements parser = defer \_ -> fold <$> someSepBy (is ";")
  (maybeToArray <$> (|?) parser)

withinContext :: forall a. Parser a -> Parser (Array a)
withinContext = withinCurlyBrackets <<< statements

withinContextTupled
  :: forall a1 a2. Parser a1 -> Parser a2 -> Parser (Array a1 /\ Array a2)
withinContextTupled p1 p2 = withinCurlyBrackets $ (/\) <$> statements p1 <*>
  statements p2

csvOf = collOf openParens closeParens comma

sepByOps :: forall a b. Parser a -> Parser b -> Parser (Array a /\ Array b)
sepByOps sep p = do
  x <- p
  y <- (|+) ((/\) <$> sep <*> p)
  pure $ ((fst <$> y) /\ Array.cons x (snd <$> y))
