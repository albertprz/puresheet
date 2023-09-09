module App.Parsers.Pattern where

import FatPrelude

import App.Parsers.Common (cellValue, isToken, token, var)
import App.SyntaxTrees.Pattern (Pattern(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators ((<|>))
import Bookhound.Parsers.Char (underscore)
import Bookhound.Parsers.Collections (listOf)
import Control.Lazy (defer)

pattern' :: Parser Pattern
pattern' = pattern''
  where
  -- ctor' = defer \_ -> CtorPattern <$> ctor <*> argListOf ctorElem
  -- nullaryCtor = defer \_ -> CtorPattern <$> ctor <*> pure []
  alias = defer \_ -> AliasedPattern <$> (var <* isToken "@") <*> aliasElem
  var' = defer \_ -> VarPattern <$> var
  cellValue' = defer \_ -> LitPattern <$> cellValue
  wildcard = defer \_ -> Wildcard <$ token underscore
  spread = defer \_ -> Spread <$ isToken "..."
  list = defer \_ -> ListPattern <$> listOf pattern'
  elem' = defer \_ -> cellValue'
    <|> var'
    <|> alias
    <|> wildcard
    <|> spread
    -- <|> nullaryCtor
    <|> list
  ctorElem = defer \_ ->
    alias
      -- <|> ctor'
      <|> elem'
  aliasElem = defer \_ -> elem'
  -- <|> withinParens ctor'
  pattern'' = defer \_ -> alias
    -- <|> ctor'
    <|> ctorElem
