module App.SyntaxTree.Type where

import FatPrelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String

newtype TypeParam = TypeParam Char

newtype TypeVar = TypeVar String

data Type
  = VarTypeApply TypeVar (Array Type)
  | ParamTypeApply TypeParam (Array Type)
  | ArrowTypeApply (Array Type)
  | UnionTypeApply (Array Type)
  | ArrayTypeApply Type
  | TypeVar' TypeVar
  | TypeParam' TypeParam

derive instance Newtype TypeVar _
instance Show TypeVar where
  show = unwrap

derive instance Newtype TypeParam _
instance Show TypeParam where
  show = String.singleton <<< unwrap

derive instance Generic Type _
instance Show Type where
  show x = genericShow x

derive instance Eq TypeVar

derive instance Eq TypeParam

derive instance Eq Type
