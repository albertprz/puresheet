module App.SyntaxTrees.Common where

import FatPrelude
import Prim hiding (Row)

import Data.String.CodeUnits as String

newtype Var = Var String

derive instance Eq Var
derive instance Ord Var
instance Show Var where
  show (Var x) = x

newtype Ctor = Ctor String

derive instance Eq Ctor
derive instance Ord Ctor
instance Show Ctor where
  show (Ctor x) = x

newtype VarOp = VarOp String

derive instance Eq VarOp
derive instance Ord VarOp
instance Show VarOp where
  show (VarOp x) = x

newtype Module = Module (Array String)

derive instance Eq Module
instance Show Module where
  show (Module x) = intercalate "." x

data Literal
  = BoolLit Boolean
  | IntLit Int
  | FloatLit Number
  | CharLit Char
  | StringLit String
  | ListLit (Array Literal)

derive instance Eq Literal
instance Show Literal where
  show = case _ of
    (BoolLit x) -> show x
    (IntLit x) -> show x
    (FloatLit x) -> show x
    (CharLit x) -> String.singleton x
    (StringLit x) -> x
    (ListLit x) -> show x
