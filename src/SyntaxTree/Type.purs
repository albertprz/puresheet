module App.SyntaxTree.Type where

import FatPrelude
import Prim hiding (Type)

import App.Utils.Char (decodeChar)
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
derive newtype instance EncodeJson TypeVar
derive newtype instance DecodeJson TypeVar

instance Show TypeVar where
  show = unwrap

derive instance Newtype TypeParam _

derive newtype instance EncodeJson TypeParam

instance DecodeJson TypeParam where
  decodeJson = map wrap <<< decodeChar

instance Show TypeParam where
  show = String.singleton <<< unwrap

derive instance Generic Type _
instance Show Type where
  show x = genericShow x

instance EncodeJson Type where
  encodeJson x = genericEncodeJson x

instance DecodeJson Type where
  decodeJson x = genericDecodeJson x

derive instance Eq TypeVar

derive instance Eq TypeParam

derive instance Eq Type
