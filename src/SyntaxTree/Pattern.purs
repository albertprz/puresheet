module App.SyntaxTree.Pattern where

import FatPrelude

import App.Components.Spreadsheet.Cell (CellValue)
import App.SyntaxTree.Common (Var)
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

instance EncodeJson Pattern where
  encodeJson x = genericEncodeJson x

instance DecodeJson Pattern where
  decodeJson x = genericDecodeJson x
