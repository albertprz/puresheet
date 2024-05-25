module App.Components.Editor.Models where

import FatPrelude

import App.AppStore (Store)
import App.Components.Spreadsheet.Formula (FormulaState)
import App.Editor.Suggestion (SuggestionId, SuggestionTerm)
import App.Utils.KeyCode (KeyCode)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type EditorState =
  { formulaState :: FormulaState
  , placeholder :: String
  , suggestions :: Array SuggestionTerm
  , selectedSuggestionId :: SuggestionId
  , store :: Store
  }

data EditorAction
  = KeyDown (Maybe SuggestionTerm) KeyCode KeyboardEvent
  | KeyUp KeyCode KeyboardEvent
  | MouseDown MouseEvent
  | FocusIn FocusEvent
  | SelectionChange
  | ClickSuggestion (Maybe SuggestionTerm) MouseEvent
  | HoverSuggestion SuggestionId MouseEvent
  | Initialize
  | Receive EditorInput

type EditorInput =
  { placeholder :: String, formulaState :: FormulaState, store :: Store }

data EditorOutput
  = FocusInEditor
  | FocusOutEditor
  | SubmitEditor String
  | GoToDefinition (Maybe SuggestionTerm)

data EditorQuery a
  = UpdateEditorContent String a
  | GetEditorContents (String -> a)
  | FocusEditor a
