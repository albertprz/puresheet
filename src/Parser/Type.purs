module App.Parser.Type where

import FatPrelude
import Prim hiding (Type)

import App.Parser.Common (argListOf, ident, isToken)
import App.SyntaxTree.Type (Type(..), TypeParam(..), TypeVar(..))
import Bookhound.Parser (Parser, satisfy)
import Bookhound.ParserCombinators (multipleSepBy)
import Bookhound.Parsers.Char (upper)
import Bookhound.Parsers.String (betweenParens, betweenSquare)
import Control.Lazy (defer)
import Data.String as String

typeParam :: Parser TypeParam
typeParam = TypeParam <$> upper

typeVar :: Parser TypeVar
typeVar = TypeVar <$> satisfy ((_ > 1) <<< String.length) (ident upper)

type' :: Parser Type
type' = defer \_ -> complexType <|> atom
  where

  typeApply = defer \_ ->
    VarTypeApply <$> typeVar <*> argListOf type'
      <|> (ParamTypeApply <$> typeParam <*> argListOf type')

  arrow = defer \_ -> ArrowTypeApply <$> multipleSepBy (isToken "->")
    (atom <|> betweenParens complexType)

  union = defer \_ -> UnionTypeApply <$> multipleSepBy (isToken "|")
    (atom <|> betweenParens complexType)

  array = defer \_ -> ArrayTypeApply <$> betweenSquare type'

  typeVar' = TypeVar' <$> typeVar
  typeParam' = TypeParam' <$> typeParam

  complexType = defer \_ -> arrow <|> union
  atom = defer \_ -> typeApply <|> typeVar' <|> typeParam' <|> array
