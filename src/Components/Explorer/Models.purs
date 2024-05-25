module App.Components.Explorer.Models where

import FatPrelude

import App.AppStore (Store)
import App.Components.Explorer.FunctionFilter (FnFilter)
import App.Components.FunctionEditor (FunctionEditorSlot)
import App.Components.OperatorEditor (OperatorEditorSlot)
import App.Components.Typeahead (TypeaheadSlot)
import App.Editor.Suggestion (SuggestionInfo)
import App.Routes (Route)
import App.SyntaxTree.Common (Module(..), QVar, QVarOp)
import App.SyntaxTree.FnDef (FnInfo, OpInfo)
import App.Utils.KeyCode (KeyCode)
import Halogen (RefLabel(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

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
  | ClickEditRow Int SuggestionInfo MouseEvent
  | ClickDeleteRow Int SuggestionInfo
  | ModifyFunction
      { initialQVar :: QVar, qVar :: QVar, fnInfo :: FnInfo }
  | ModifyOperator
      { initialQVarOp :: QVarOp, qVarOp :: QVarOp, opInfo :: OpInfo }
  | CreateFunction
  | CreateOperator
  | RowKeyDown SuggestionInfo KeyCode KeyboardEvent
  | ClosedModal

type Slots =
  ( moduleTypeahead :: TypeaheadSlot Module
  , functionEditor :: FunctionEditorSlot
  , operatorEditor :: OperatorEditorSlot
  )

allModules :: Module
allModules = Module [ "All modules" ]

functionFilterInputRef :: RefLabel
functionFilterInputRef = RefLabel "functionFilterInput"

_moduleTypeahead :: Proxy "moduleTypeahead"
_moduleTypeahead = Proxy
