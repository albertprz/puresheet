module App.SyntaxTrees.Common where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Var = Var String

derive instance Eq Var
derive instance Generic Var _
instance Show Var where
  show = genericShow

newtype Ctor = Ctor String

derive instance Eq Ctor
derive instance Generic Ctor _
instance Show Ctor where
  show = genericShow

newtype VarOp = VarOp String

derive instance Eq VarOp
derive instance Generic VarOp _
instance Show VarOp where
  show = genericShow

newtype CtorOp = CtorOp String

derive instance Eq CtorOp
derive instance Generic CtorOp _
instance Show CtorOp where
  show = genericShow

newtype Module = Module (Array String)

derive instance Eq Module
derive instance Generic Module _
instance Show Module where
  show = genericShow

data Literal
  = BoolLit Boolean
  | IntLit String
  | FloatLit String
  | CharLit Char
  | StringLit String
  | CellLit Cell

derive instance Eq Literal
derive instance Generic Literal _
instance Show Literal where
  show = genericShow

data QVar = QVar (Maybe Module) Var

derive instance Eq QVar
derive instance Generic QVar _
instance Show QVar where
  show = genericShow

data QCtor = QCtor (Maybe Module) Ctor

derive instance Eq QCtor
derive instance Generic QCtor _
instance Show QCtor where
  show = genericShow

data QVarOp = QVarOp (Maybe Module) VarOp

derive instance Eq QVarOp
derive instance Generic QVarOp _
instance Show QVarOp where
  show = genericShow

data QCtorOp = QCtorOp (Maybe Module) CtorOp

derive instance Eq QCtorOp
derive instance Generic QCtorOp _
instance Show QCtorOp where
  show = genericShow
