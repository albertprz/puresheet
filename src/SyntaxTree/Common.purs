module App.SyntaxTree.Common where

import FatPrelude
import Prim hiding (Row)

newtype Var = Var String

derive newtype instance Eq Var
derive newtype instance Ord Var
derive newtype instance Show Var

newtype VarOp = VarOp String

derive newtype instance Eq VarOp
derive newtype instance Ord VarOp
derive newtype instance Show VarOp

newtype Module = Module (Array String)

derive newtype instance Eq Module
derive newtype instance Ord Module
instance Show Module where
  show (Module x) = intercalate "." x

data QVar = QVar (Maybe Module) Var

derive instance Eq QVar
derive instance Ord QVar
instance Show QVar where
  show (QVar mod x) = foldMap ((_ <> ".") <<< show) mod <> show x

data QVarOp = QVarOp (Maybe Module) VarOp

instance Show QVarOp where
  show (QVarOp mod x) = foldMap ((_ <> ".") <<< show) mod <> show x

derive instance Eq QVarOp
derive instance Ord QVarOp

preludeModule :: Module
preludeModule = Module [ "Prelude" ]
