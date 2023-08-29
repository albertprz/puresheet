module App.SyntaxTrees.Pattern where

import FatPrelude

import App.SyntaxTrees.Common (Literal, QCtor, QCtorOp, Var)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Pattern
  = CtorPattern QCtor (Array Pattern)
  | InfixCtorPattern QCtorOp (Array Pattern)
  | RecordPattern QCtor (Array (Var /\ Maybe Pattern))
  | AliasedPattern Var Pattern
  | ListPattern (Array Pattern)
  | VarPattern Var
  | LitPattern Literal
  | Wildcard

derive instance Generic Pattern _

instance Show Pattern where
  show x = genericShow x
