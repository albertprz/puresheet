module App.Parsers.Pattern where

import FatPrelude

import App.Parsers.Common (argListOf, cellValue, ctor, token, var)
import App.SyntaxTrees.Pattern (Pattern(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (anySepBy, is, (<|>), (|?))
import Bookhound.Parsers.Char (comma, underscore)
import Bookhound.Parsers.Collections (listOf)
import Bookhound.Parsers.String (withinCurlyBrackets, withinParens)
import Control.Lazy (defer)

pattern' :: Parser Pattern
pattern' = pattern''
  where
  ctor' = defer \_ -> CtorPattern <$> ctor <*> argListOf ctorElem
  nullaryCtor = defer \_ -> CtorPattern <$> ctor <*> pure []
  alias = defer \_ -> AliasedPattern <$> (var <* is "@") <*> aliasElem
  var' = defer \_ -> VarPattern <$> var
  cellValue' = defer \_ -> LitPattern <$> cellValue
  wildcard = defer \_ -> Wildcard <$ token underscore
  list = defer \_ -> ListPattern <$> listOf pattern'
  recordField = defer \_ -> (/\) <$> var <*> (|?) (is "=" *> pattern'')
  recordShape = defer \_ -> withinCurlyBrackets (anySepBy comma recordField)
  elem' = defer \_ -> cellValue'
    <|> var'
    <|> alias
    <|> wildcard
    <|> nullaryCtor
    <|> list
  ctorElem = defer \_ -> record
    <|> alias
    <|> ctor'
    <|> elem'
  aliasElem = defer \_ -> elem'
    <|> record
    <|> withinParens complexPattern
  record = defer \_ ->
    RecordPattern
      <$> ctor
      <*> recordShape
  complexPattern = defer \_ -> ctor'
  pattern'' = defer \_ -> alias <|> ctor' <|> ctorElem
