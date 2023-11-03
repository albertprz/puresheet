module App.Parser.Type where

import Prim hiding (Type)

import App.Parser.Common (argListOf, ident, isToken)
import App.SyntaxTree.Type (Type(..), TypeParam(..), TypeVar(..))
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (multipleSepBy, (<|>))
import Bookhound.Parsers.Char (upper)
import Bookhound.Parsers.String (withinParens, withinSquareBrackets)
import Control.Lazy (defer)
import FatPrelude ((<$>), (<*>))

typeParam :: Parser TypeParam
typeParam = TypeParam <$> upper

typeVar :: Parser TypeVar
typeVar = TypeVar <$> ident upper

type' :: Parser Type
type' = defer \_ -> arrow <|> union <|> atom
  where

  typeApply = defer \_ ->
    CtorTypeApply <$> typeVar <*> argListOf type'
      <|> (ParamTypeApply <$> typeParam <*> argListOf type')

  arrow = defer \_ -> ArrowTypeApply <$> multipleSepBy (isToken "->")
    (atom <|> union <|> withinParens arrow)

  union = defer \_ -> UnionTypeApply <$> multipleSepBy (isToken "|")
    (atom <|> arrow <|> withinParens union)

  array = defer \_ -> ArrayTypeApply <$> withinSquareBrackets type'

  typeVar' = TypeVar' <$> typeVar
  typeParam' = TypeParam' <$> typeParam

  atom = defer \_ -> typeVar' <|> typeParam' <|> typeApply <|> array
