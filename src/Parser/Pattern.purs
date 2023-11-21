module App.Parser.Pattern where

import FatPrelude

import App.Parser.Common (cellValue, isToken, token, var)
import App.SyntaxTree.Pattern (Pattern(..))
import Bookhound.Parser (Parser)
import Bookhound.Parsers.Char (underscore)
import Bookhound.Parsers.Collections (listOf)
import Bookhound.Parsers.String (betweenParens)
import Control.Lazy (defer)

pattern' :: Parser Pattern
pattern' = openForm
  where
  alias = defer \_ -> AliasedPattern <$> (var <* isToken "@") <*> infixArgForm
  var' = defer \_ -> VarPattern <$> var
  literal = defer \_ -> LitPattern <$> cellValue
  wildcard = defer \_ -> Wildcard <$ token underscore
  spread = defer \_ -> Spread <$ isToken "..."
  list = defer \_ -> ArrayPattern <$> listOf pattern'
  singleForm = defer \_ ->
    wildcard
      <|> spread
      <|> literal
      <|> list
      <|> var'
  complexForm = defer \_ -> alias <|> complexInfixForm
  complexInfixForm = defer \_ ->
    betweenParens alias
  infixArgForm = defer \_ ->
    complexInfixForm
      <|> betweenParens complexInfixForm
      <|> singleForm
  openForm = defer \_ -> complexForm <|> singleForm
    <|> betweenParens (complexForm <|> singleForm)
