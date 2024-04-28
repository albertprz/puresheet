module App.Components.Explorer.Models where

import FatPrelude

import App.AppStore (Store)
import App.Components.Explorer.FunctionFilter (FnFilter)
import App.Components.Typeahead (TypeaheadSlot)
import App.Routes (Route)
import App.SyntaxTree.Common (Module(..))
import App.Utils.KeyCode (KeyCode)
import Halogen (RefLabel(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type ExplorerState =
  { route :: Route
  , store :: Store
  , module' :: Maybe Module
  , fnFilter :: Maybe FnFilter
  , fnFilterText :: String
  , selectedRow :: Int
  }

type ExplorerInput = { route :: Route }

data ExplorerAction
  = Initialize
  | Receive { input :: ExplorerInput, context :: Store }
  | SelectFunctionRow Int
  | KeyDown KeyCode KeyboardEvent
  | TableKeyDown KeyCode KeyboardEvent
  | FunctionFilterKeyDown KeyCode KeyboardEvent
  | ClickFunctionRow Int
  | SelectModule (Maybe Module)
  | UpdateFunctionFilter (Maybe FnFilter)

type Slots = (moduleTypeahead :: TypeaheadSlot Module)

allModules :: Module
allModules = Module [ "All modules" ]

functionFilterInputRef :: RefLabel
functionFilterInputRef = RefLabel "functionFilterInput"

_moduleTypeahead :: Proxy "moduleTypeahead"
_moduleTypeahead = Proxy
