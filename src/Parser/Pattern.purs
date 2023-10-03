module App.Parser.Pattern where

import FatPrelude

import App.Parser.Common (cellValue, isToken, token, var)
import App.SyntaxTree.Pattern (Pattern(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators ((<|>))
import Bookhound.Parsers.Char (underscore)
import Bookhound.Parsers.Collections (listOf)
import Control.Lazy (defer)

pattern' :: Parser Pattern
pattern' = pattern''
  where
  alias = defer \_ -> AliasedPattern <$> (var <* isToken "@") <*> aliasElem
  var' = defer \_ -> VarPattern <$> var
  cellValue' = defer \_ -> LitPattern <$> cellValue
  wildcard = defer \_ -> Wildcard <$ token underscore
  spread = defer \_ -> Spread <$ isToken "..."
  list = defer \_ -> ArrayPattern <$> listOf pattern'
  elem' = defer \_ -> cellValue'
    <|> var'
    <|> alias
    <|> wildcard
    <|> spread
    <|> list
  ctorElem = defer \_ ->
    alias
      <|> elem'
  aliasElem = defer \_ -> elem'
  pattern'' = defer \_ -> alias
    <|> ctorElem
