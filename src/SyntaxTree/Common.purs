module App.SyntaxTree.Common where

import FatPrelude
import Prim hiding (Row)

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

newtype Var = Var String

derive newtype instance Eq Var
derive newtype instance Ord Var
derive instance Newtype Var _
derive newtype instance EncodeJson Var
derive newtype instance DecodeJson Var

instance Show Var where
  show = unwrap

instance Hashable Var where
  hash = hash <<< unwrap

newtype VarOp = VarOp String

derive newtype instance Eq VarOp
derive newtype instance Ord VarOp
derive instance Newtype VarOp _
derive newtype instance EncodeJson VarOp
derive newtype instance DecodeJson VarOp

instance Show VarOp where
  show = unwrap

newtype Module = Module (Array String)

derive newtype instance Eq Module
derive newtype instance Ord Module
derive instance Newtype Module _
derive newtype instance EncodeJson Module
derive newtype instance DecodeJson Module

instance Show Module where
  show (Module x) = intercalate "." x

instance Hashable Module where
  hash = hash <<< show

data QVar = QVar (Maybe Module) Var

derive instance Eq QVar
derive instance Ord QVar
derive instance Generic QVar _

instance EncodeJson QVar where
  encodeJson = genericEncodeJson

instance DecodeJson QVar where
  decodeJson = genericDecodeJson

instance Show QVar where
  show (QVar mod x) = foldMap ((_ <> ".") <<< show) mod <> show x

instance Hashable QVar where
  hash = hash <<< show

data QVarOp = QVarOp (Maybe Module) VarOp

derive instance Eq QVarOp
derive instance Ord QVarOp
derive instance Generic QVarOp _

instance EncodeJson QVarOp where
  encodeJson = genericEncodeJson

instance DecodeJson QVarOp where
  decodeJson = genericDecodeJson

instance Show QVarOp where
  show (QVarOp mod x) = foldMap ((_ <> ".") <<< show) mod <> show x

instance Hashable QVarOp where
  hash = hash <<< show

preludeModule :: Module
preludeModule = Module [ "Prelude" ]
