module App.Components.Editor.Handler where

import FatPrelude

import App.CSS.Ids (formulaBoxId, formulaCellInputId)
import App.Components.Editor.HandlerHelpers (displayFnSig, displayFnSuggestions, getEditorContent, insertEditorNewLine, performAutoComplete, performSyntaxHighlight, subscribeSelectionChange, updateEditorContent)
import App.Components.Editor.Models (EditorAction(..), EditorOutput(..), EditorQuery(..), EditorState)
import App.Components.Table.Formula (FormulaState(..))
import App.Utils.Dom (actOnElementById, focusById, withPrevent)
import App.Utils.Event (ctrlKey, toEvent)
import App.Utils.KeyCode (KeyCode(..), isModifierKeyCode)
import App.Utils.Selection as Selection
import Halogen (HalogenM, raise)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLElement (fromEventTarget, setContentEditable, toNode)

handleAction
  :: forall m
   . MonadAff m
  => EditorAction
  -> HalogenM EditorState EditorAction () EditorOutput m Unit

handleAction Initialize = do
  actOnElementById formulaBoxId $ setContentEditable "true"
  subscribeSelectionChange

handleAction (KeyDown (Just _) keyCode ev)
  | keyCode `elem` [ ArrowUp, ArrowDown ] = withPrevent ev do
      let next = if keyCode == ArrowUp then dec else inc
      modify_ \st -> st
        { selectedSuggestionId =
            clamp bottom
              (wrap $ dec $ length st.suggestions)
              $ next st.selectedSuggestionId
        }

handleAction (KeyDown (Just suggestion) keyCode ev)
  | keyCode `elem` [ Enter, Tab ] =
      withPrevent ev $ performAutoComplete $ show suggestion

handleAction (KeyDown _ Enter ev)
  | ctrlKey ev = withPrevent ev do
      editorText <- getEditorContent
      raise $ SubmitEditor editorText
  | otherwise = withPrevent ev do
      insertEditorNewLine
      performSyntaxHighlight

handleAction (KeyDown _ Tab ev) = withPrevent ev do
  modify_ _ { suggestions = [] }
  raise FocusOutEditor

handleAction (KeyDown _ (CharKeyCode 'G') ev)
  | ctrlKey ev = withPrevent ev $ focusById formulaCellInputId

handleAction (KeyDown _ _ _) =
  modify_ _ { formulaState = UnknownFormula }

handleAction (KeyUp keyCode _) =
  unless (isModifierKeyCode keyCode)
    performSyntaxHighlight

handleAction (FocusIn ev) = do
  selection <- liftEffect $ Selection.getSelection =<< window
  liftEffect $ traverse_ (Selection.moveToEnd selection) formulaBox
  raise FocusInEditor
  where
  formulaBox =
    toNode <$> (fromEventTarget =<< target (toEvent ev))

handleAction SelectionChange = do
  displayFnSuggestions
  displayFnSig =<< get

handleAction (Receive { formulaState }) =
  modify_ _ { formulaState = formulaState }

handleQuery
  :: forall a m
   . MonadEffect m
  => EditorQuery a
  -> HalogenM EditorState EditorAction () EditorOutput m (Maybe a)
handleQuery (UpdateEditorContent text next) = do
  updateEditorContent text
  pure $ Just next
