module App.SyntaxTree.Common where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, Column, Row)

newtype Var = Var String

derive instance Eq Var

newtype Ctor = Ctor String

derive instance Eq Ctor

newtype VarOp = VarOp String

derive instance Eq VarOp

newtype CtorOp = CtorOp String

derive instance Eq CtorOp

newtype Module = Module (Array String)

derive instance Eq Module

data Literal
  = BoolLit Boolean
  | IntLit String
  | FloatLit String
  | CharLit Char
  | StringLit String
  | ColumnLit Column
  | RowLit Row
  | CellLit Cell

derive instance Eq Literal

data QVar = QVar (Maybe Module) Var

derive instance Eq QVar

data QCtor = QCtor (Maybe Module) Ctor

derive instance Eq QCtor

data QVarOp = QVarOp (Maybe Module) VarOp

derive instance Eq QVarOp

data QCtorOp = QCtorOp (Maybe Module) CtorOp

derive instance Eq QCtorOp
