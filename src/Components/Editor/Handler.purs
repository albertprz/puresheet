module App.Components.Editor.Handler where

import FatPrelude

import App.AppM (AppM)
import App.CSS.Ids (formulaCellInputId)
import App.Components.Editor.HandlerHelpers (displayFnSig, displayFnSuggestions, getEditorContent, getTermAtCaret, insertEditorNewLine, performAutoComplete, performSyntaxHighlight, subscribeSelectionChange, updateEditorContent)
import App.Components.Editor.Models (EditorAction(..), EditorOutput(..), EditorQuery(..), EditorState)
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Routes (Route(..))
import App.Utils.Dom (focusById, withPrevent)
import App.Utils.Event (ctrlKey, shiftKey, toEvent)
import App.Utils.KeyCode (KeyCode(..), isModifierKeyCode)
import App.Utils.Selection as Selection
import Data.Array as Array
import Halogen (HalogenM, raise)
import Halogen.Router.Class (navigate)
import Halogen.Store.Monad (getStore)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLElement (fromEventTarget, toNode)

handleAction
  :: EditorAction
  -> HalogenM EditorState EditorAction () EditorOutput AppM Unit

handleAction (KeyDown (Just suggestion) Enter ev) = withPrevent ev do
  performAutoComplete suggestion

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
      traverse_ (navigate <<< ExplorerView <<< { selectedTerm: _ } <<< Just)
        =<< getTermAtCaret

handleAction (KeyDown _ _ _) =
  modify_ _ { formulaState = UnknownFormula }

handleAction (KeyUp keyCode _) =
  unless (isModifierKeyCode keyCode)
    performSyntaxHighlight

handleAction (MouseDown ev) = when (ctrlKey ev) do
  traverse_ (navigate <<< ExplorerView <<< { selectedTerm: _ } <<< Just)
    =<< getTermAtCaret

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
  st <- get
  store <- getStore
  displayFnSuggestions
  displayFnSig st store

handleAction Initialize =
  subscribeSelectionChange

handleAction (Receive { formulaState }) =
  modify_ _ { formulaState = formulaState }

handleQuery
  :: forall a
   . EditorQuery a
  -> HalogenM EditorState EditorAction () EditorOutput AppM (Maybe a)
handleQuery (UpdateEditorContent text next) = do
  updateEditorContent text
  pure $ Just next
