module App.SyntaxTrees.Pattern where

import FatPrelude

import App.SyntaxTrees.Common (Ctor, Literal, Var)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Pattern
  = CtorPattern Ctor (Array Pattern)
  | RecordPattern Ctor (Array (Var /\ Maybe Pattern))
  | AliasedPattern Var Pattern
  | ListPattern (Array Pattern)
  | VarPattern Var
  | LitPattern Literal
  | Wildcard

derive instance Generic Pattern _

instance Show Pattern where
  show x = genericShow x
