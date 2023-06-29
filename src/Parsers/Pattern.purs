module App.Parsers.Pattern where

import FatPrelude

import App.Parsers.Common (literal, qCtor, qCtorOp, token, var)
import App.SyntaxTrees.Pattern (Pattern(..), ctorPattern, infixCtorPattern, recordPattern)
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (anySepBy, is, sepByOp, (<|>), (|+), (|?))
import Bookhound.Parsers.Char (comma, underscore)
import Bookhound.Parsers.Collections (listOf, tupleOf)
import Bookhound.Parsers.String (maybeWithinParens, withinCurlyBrackets, withinParens)
import Control.Lazy (defer)

pattern' :: Parser Pattern
pattern' = pattern'' <|> maybeWithinParens pattern''
  where
  ctor' = defer \_ -> ctorPattern <$> qCtor <*> (|+) ctorElem
  nullaryCtor = defer \_ -> ctorPattern <$> qCtor <*> pure []
  infixCtor = defer \_ -> uncurry infixCtorPattern
    <$> sepByOp qCtorOp (ctor' <|> ctorElem)
  alias = defer \_ -> AliasedPattern <$> (var <* is "@") <*> aliasElem
  var' = defer \_ -> VarPattern <$> var
  literal' = defer \_ -> LitPattern <$> literal
  wildcard = defer \_ -> Wildcard <$ token underscore
  list = defer \_ -> ListPattern <$> listOf pattern'
  tuple = defer \_ -> TuplePattern <$> tupleOf pattern'
  recordField = defer \_ -> (/\) <$> var <*> (|?) (is "=" *> pattern'')
  recordShape = defer \_ -> withinCurlyBrackets (anySepBy comma recordField)
  elem = defer \_ -> literal'
    <|> var'
    <|> alias
    <|> wildcard
    <|> nullaryCtor
    <|> withinParens nullaryCtor
    <|> tuple
    <|> list
  ctorElem = defer \_ ->
    record
      <|> alias
      <|> elem
      <|> withinParens complexPattern
  aliasElem = defer \_ ->
    elem
      <|> record
      <|> withinParens complexPattern
  record = defer \_ -> maybeWithinParens
    $ recordPattern
    <$> qCtor
    <*> recordShape
  complexPattern = defer \_ -> ctor' <|> infixCtor
  pattern'' = defer \_ -> alias <|> infixCtor <|> ctor' <|> ctorElem
