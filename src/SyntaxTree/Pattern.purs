module App.SyntaxTree.Pattern where

import FatPrelude

import App.Components.Table.Cell (CellValue)
import App.SyntaxTree.Common (Var)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Pattern
  = VarPattern Var
  | LitPattern CellValue
  | AliasedPattern Var Pattern
  | ArrayPattern (Array Pattern)
  | Wildcard
  | Spread

derive instance Eq Pattern
derive instance Generic Pattern _

instance Show Pattern where
  show x = genericShow x
