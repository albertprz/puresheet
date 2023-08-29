module App.Parsers.Pattern where

import FatPrelude

import App.Parsers.Common (literal, qCtor, qCtorOp, token, var)
import App.SyntaxTrees.Pattern (Pattern(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (anySepBy, is, sepByOp, someSepBy, (<|>), (|?))
import Bookhound.Parsers.Char (comma, underscore)
import Bookhound.Parsers.Collections (listOf)
import Bookhound.Parsers.String (withinCurlyBrackets, withinParens)
import Control.Lazy (defer)

pattern' :: Parser Pattern
pattern' = pattern''
  where
  ctor' = defer \_ -> CtorPattern <$> qCtor <*> argListOf ctorElem
  nullaryCtor = defer \_ -> CtorPattern <$> qCtor <*> pure []
  infixCtor = defer \_ -> uncurry InfixCtorPattern
    <$> sepByOp qCtorOp (ctor' <|> ctorElem)
  alias = defer \_ -> AliasedPattern <$> (var <* is "@") <*> aliasElem
  var' = defer \_ -> VarPattern <$> var
  literal' = defer \_ -> LitPattern <$> literal
  wildcard = defer \_ -> Wildcard <$ token underscore
  list = defer \_ -> ListPattern <$> listOf pattern'
  recordField = defer \_ -> (/\) <$> var <*> (|?) (is "=" *> pattern'')
  recordShape = defer \_ -> withinCurlyBrackets (anySepBy comma recordField)
  elem' = defer \_ -> literal'
    <|> var'
    <|> alias
    <|> wildcard
    <|> nullaryCtor
    <|> list
  ctorElem = defer \_ -> record
    <|> alias
    <|> ctor'
    <|> elem'
    <|> infixCtor
  aliasElem = defer \_ -> elem'
    <|> record
    <|> withinParens complexPattern
  record = defer \_ ->
    RecordPattern
      <$> qCtor
      <*> recordShape
  complexPattern = defer \_ -> ctor' <|> infixCtor
  pattern'' = defer \_ -> alias <|> infixCtor <|> ctor' <|> ctorElem

argListOf :: forall a. Parser a -> Parser (Array a)
argListOf = withinParens <<< someSepBy comma
