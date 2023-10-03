module App.SyntaxTree.Common where

import FatPrelude
import Prim hiding (Row)

newtype Var = Var String

derive instance Eq Var
derive instance Ord Var
instance Show Var where
  show (Var x) = x

newtype VarOp = VarOp String

derive instance Eq VarOp
derive instance Ord VarOp
instance Show VarOp where
  show (VarOp x) = x

newtype Module = Module (Array String)

derive instance Eq Module
derive instance Ord Module
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
