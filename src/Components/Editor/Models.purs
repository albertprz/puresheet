module App.Components.Editor.Models where

import FatPrelude

import App.Components.Table.Formula (FormulaState)
import App.Editor.Formula (SuggestionId, SuggestionTerm)
import App.Utils.KeyCode (KeyCode)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type EditorState =
  { formulaState :: FormulaState
  , suggestions :: Array SuggestionTerm
  , selectedSuggestionId :: SuggestionId
  }

data EditorAction
  = Initialize
  | KeyDown (Maybe SuggestionTerm) KeyCode KeyboardEvent
  | KeyUp KeyCode KeyboardEvent
  | FocusIn FocusEvent
  | SelectionChange
  | Receive EditorInput

type EditorInput = { formulaState :: FormulaState }

data EditorOutput
  = FocusInEditor
  | FocusOutEditor
  | SubmitEditor String

data EditorQuery a =
  UpdateEditorContent String a
