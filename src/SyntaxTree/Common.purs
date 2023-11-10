module App.SyntaxTree.Common where

import FatPrelude
import Prim hiding (Row)

newtype Var = Var String

derive newtype instance Eq Var
derive newtype instance Ord Var
derive instance Newtype Var _
instance Show Var where
  show = unwrap

instance Hashable Var where
  hash = hash <<< unwrap

newtype VarOp = VarOp String

derive newtype instance Eq VarOp
derive newtype instance Ord VarOp
derive instance Newtype VarOp _
instance Show VarOp where
  show = unwrap

newtype Module = Module (Array String)

derive newtype instance Eq Module
derive newtype instance Ord Module
instance Show Module where
  show (Module x) = intercalate "." x

instance Hashable Module where
  hash = hash <<< show

data QVar = QVar (Maybe Module) Var

derive instance Eq QVar
derive instance Ord QVar
instance Show QVar where
  show (QVar mod x) = foldMap ((_ <> ".") <<< show) mod <> show x

instance Hashable QVar where
  hash = hash <<< show

data QVarOp = QVarOp (Maybe Module) VarOp

derive instance Eq QVarOp
derive instance Ord QVarOp

instance Show QVarOp where
  show (QVarOp mod x) = foldMap ((_ <> ".") <<< show) mod <> show x

instance Hashable QVarOp where
  hash = hash <<< show

preludeModule :: Module
preludeModule = Module [ "Prelude" ]
