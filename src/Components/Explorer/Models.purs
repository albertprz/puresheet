module App.Components.Explorer.Models where

import FatPrelude

import App.AppStore (Store)
import App.Routes (Route)
import App.SyntaxTree.Common (Module)

type ExplorerState =
  { route :: Route
  , store :: Store
  , module' :: Maybe Module
  , fnInput :: String
  , selectedRowNumber :: Int
  }

type ExplorerInput = { route :: Route }

data ExplorerAction
  = Receive { input :: ExplorerInput, context :: Store }
  | ClickFunctionRow Int
