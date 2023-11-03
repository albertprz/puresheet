module App.SyntaxTree.Type where

import FatPrelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype TypeParam = TypeParam Char

newtype TypeVar = TypeVar String

data Type
  = CtorTypeApply TypeVar (Array Type)
  | ParamTypeApply TypeParam (Array Type)
  | ArrowTypeApply (Array Type)
  | UnionTypeApply (Array Type)
  | ArrayTypeApply Type
  | TypeVar' TypeVar
  | TypeParam' TypeParam

derive newtype instance Show TypeVar
derive newtype instance Show TypeParam

derive instance Generic Type _
instance Show Type where
  show x = genericShow x
