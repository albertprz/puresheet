module App.Components.Editor.Models where

import FatPrelude

import App.Components.Spreadsheet.Formula (FormulaState)
import App.Editor.Suggestion (SuggestionId, SuggestionTerm)
import App.Utils.KeyCode (KeyCode)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type EditorState =
  { formulaState :: FormulaState
  , suggestions :: Array SuggestionTerm
  , selectedSuggestionId :: SuggestionId
  }

data EditorAction
  = KeyDown (Maybe SuggestionTerm) KeyCode KeyboardEvent
  | KeyUp KeyCode KeyboardEvent
  | FocusIn FocusEvent
  | SelectionChange
  | ClickSuggestion (Maybe SuggestionTerm) MouseEvent
  | HoverSuggestion SuggestionId MouseEvent
  | Initialize
  | Receive EditorInput

type EditorInput = { formulaState :: FormulaState }

data EditorOutput
  = FocusInEditor
  | FocusOutEditor
  | SubmitEditor String

data EditorQuery a =
  UpdateEditorContent String a
