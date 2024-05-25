module App.Components.Editor.Handler where

import FatPrelude

import App.CSS.Ids (formulaCellInputId)
import App.Components.Editor.HandlerHelpers (displayFnSig, displayFnSuggestions, getEditorContent, getTermAtCaret, insertEditorNewLine, performAutoComplete, performSyntaxHighlight, subscribeSelectionChange, updateEditorContent)
import App.Components.Editor.Models (EditorAction(..), EditorOutput(..), EditorQuery(..), EditorState)
import App.Components.Editor.Renderer (formulaBoxRef)
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Utils.Dom (focusById, focusRef, stopPropagation, withPrevent)
import App.Utils.Event (ctrlKey, shiftKey, toEvent)
import App.Utils.KeyCode (KeyCode(..), isModifierKeyCode)
import App.Utils.Selection as Selection
import Data.Array as Array
import Halogen (HalogenM, raise)
import Record (merge)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLElement (fromEventTarget, toNode)

handleAction
  :: EditorAction
  -> HalogenM EditorState EditorAction () EditorOutput Aff Unit

handleAction (KeyDown (Just suggestion) Enter ev) = withPrevent ev do
  when (not ctrlKey ev) $ performAutoComplete suggestion

handleAction (KeyDown (Just suggestion) Tab ev) = withPrevent ev do
  { suggestions } <- get
  let
    keyCode
      | Array.length suggestions == one = Enter
      | shiftKey ev = ArrowUp
      | otherwise = ArrowDown
  handleAction (KeyDown (Just suggestion) keyCode ev)

handleAction (KeyDown (Just _) keyCode ev)
  | keyCode `elem` [ ArrowUp, ArrowDown ] = withPrevent ev do
      let next = if keyCode == ArrowUp then dec else inc
      modify_ \st -> st
        { selectedSuggestionId =
            clamp bottom
              (wrap $ dec $ length st.suggestions)
              $ next st.selectedSuggestionId
        }

handleAction (KeyDown Nothing keyCode ev)
  | keyCode `elem` [ ArrowUp, ArrowDown ] = stopPropagation ev

handleAction (KeyDown _ Enter ev)
  | ctrlKey ev = withPrevent ev do
      editorText <- getEditorContent
      raise $ SubmitEditor editorText
  | otherwise = withPrevent ev do
      insertEditorNewLine
      performSyntaxHighlight

handleAction (KeyDown _ Tab ev) =
  withPrevent ev $ raise FocusOutEditor

handleAction (KeyDown _ (CharKeyCode 'G') ev)
  | ctrlKey ev = withPrevent ev $ focusById formulaCellInputId

handleAction (KeyDown _ (CharKeyCode 'D') ev)
  | ctrlKey ev = withPrevent ev do
      term <- getTermAtCaret
      raise $ GoToDefinition term

handleAction (KeyDown _ Delete _) =
  modify_ _ { formulaState = UnknownFormula }

handleAction (KeyDown _ (CharKeyCode ch) ev) =
  unless (ctrlKey ev && ch `notElem` [ 'X', 'V' ])
    $ modify_ _ { formulaState = UnknownFormula }

handleAction (KeyDown _ _ _) =
  pure unit

handleAction (KeyUp keyCode _) =
  unless (isModifierKeyCode keyCode)
    performSyntaxHighlight

handleAction (MouseDown ev) =
  when (ctrlKey ev) do
    term <- getTermAtCaret
    raise $ GoToDefinition term

handleAction (FocusIn ev) = do
  selection <- liftEffect $ Selection.getSelection =<< window
  liftEffect $ traverse_ (Selection.moveToEnd selection) formulaBox
  raise FocusInEditor
  where
  formulaBox =
    toNode <$> (fromEventTarget =<< target (toEvent ev))

handleAction (ClickSuggestion suggestion ev) =
  withPrevent ev $ traverse_ performAutoComplete suggestion

handleAction (HoverSuggestion suggestionId _) =
  modify_ _ { selectedSuggestionId = suggestionId }

handleAction SelectionChange = do
  st@{ store } <- get
  displayFnSuggestions store
  displayFnSig st store

handleAction Initialize =
  subscribeSelectionChange

handleAction (Receive input) =
  modify_ (merge input)

handleQuery
  :: forall a
   . EditorQuery a
  -> HalogenM EditorState EditorAction () EditorOutput Aff (Maybe a)
handleQuery (UpdateEditorContent text next) = do
  updateEditorContent text
  pure $ Just next

handleQuery (GetEditorContents reply) = do
  text <- getEditorContent
  pure $ Just $ reply text

handleQuery (FocusEditor next) = do
  focusRef formulaBoxRef
  pure $ Just next
