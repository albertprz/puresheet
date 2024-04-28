module App.Components.Explorer.Handler where

import FatPrelude

import App.AppM (AppM)
import App.CSS.Ids (functionRowId)
import App.Components.Explorer.FunctionFilter (FnFilter(..))
import App.Components.Explorer.Models (ExplorerAction(..), ExplorerState, Slots, _moduleTypeahead, allModules, functionFilterInputRef)
import App.Components.Typeahead (TypeaheadOutput(..), TypeaheadQuery(..))
import App.Editor.Suggestion (SuggestionTerm(..))
import App.Routes (Route(..))
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..), preludeModule)
import App.Utils.Dom (focusById, focusRef, scrollById, selectElementById, withPrevent)
import App.Utils.Event (ctrlKey, shiftKey)
import App.Utils.KeyCode (KeyCode(..))
import Halogen (HalogenM)
import Halogen.Query (tellAll)

handleAction
  :: ExplorerAction
  -> HalogenM ExplorerState ExplorerAction Slots Unit AppM Unit

handleAction Initialize =
  refreshFocusAndScroll

handleAction (Receive { input, context }) = do
  handleNewRoute input.route
  modify_ _ { route = input.route, store = context }
  refreshFocusAndScroll

handleAction (KeyDown (CharKeyCode 'M') ev)
  | ctrlKey ev = withPrevent ev
      $ tellAll _moduleTypeahead ActivateTypeahead

handleAction (KeyDown (CharKeyCode 'F') ev)
  | ctrlKey ev = withPrevent ev
      $ focusRef functionFilterInputRef

handleAction (KeyDown _ _) = pure unit

handleAction (TableKeyDown keyCode ev)
  | keyCode == ArrowUp || keyCode == Tab && shiftKey ev =
      withPrevent ev do
        n <- gets _.selectedRow
        handleAction $ SelectFunctionRow $ dec n
  | keyCode == ArrowDown || keyCode == Tab =
      withPrevent ev do
        n <- gets _.selectedRow
        handleAction $ SelectFunctionRow $ inc n
  | otherwise = pure unit

handleAction (FunctionFilterKeyDown keyCode ev) =
  when (keyCode `elem` [ Enter, Tab ]) $ withPrevent ev $ refreshFocus

handleAction (SelectFunctionRow n) =
  whenM (map isJust $ selectElementById $ functionRowId n) do
    handleAction $ ClickFunctionRow n
    scrollById $ functionRowId n

handleAction (ClickFunctionRow n) = do
  modify_ _ { selectedRow = n }
  refreshFocus

handleAction (SelectModule module') = do
  modify_ _ { module' = module' }
  refreshFocus

handleAction (UpdateFunctionFilter fnFilter) = do
  currentFnFilter <- gets _.fnFilter
  when (fnFilter /= currentFnFilter)
    (modify_ _ { fnFilter = fnFilter, selectedRow = zero })

refreshFocus :: forall s. HalogenM ExplorerState ExplorerAction s Unit AppM Unit
refreshFocus = do
  n <- gets _.selectedRow
  focusById $ functionRowId n

refreshFocusAndScroll
  :: forall s. HalogenM ExplorerState ExplorerAction s Unit AppM Unit
refreshFocusAndScroll = do
  n <- gets _.selectedRow
  focusById $ functionRowId n
  scrollById $ functionRowId n

handleNewRoute
  :: Route -> HalogenM ExplorerState ExplorerAction Slots Unit AppM Unit
handleNewRoute (ExplorerView { selectedTerm: Just selectedTerm }) = do
  tellAll _moduleTypeahead $ SelectOption $ fromMaybe preludeModule module'
  modify_ _ { fnFilterText = foldMap show fnFilter }
  handleAction $ UpdateFunctionFilter fnFilter
  where
  module' /\ fnFilter = case selectedTerm of
    ModuleSuggestion x -> Just x /\ Nothing
    FnSuggestion (QVar x y) -> x /\ Just (FnName $ QVar Nothing y)
    BuiltinFnSuggestion (QVar x y) -> x /\ Just (FnName $ QVar Nothing y)
    OpSuggestion (QVarOp x y) -> x /\ Just (OpName $ QVarOp Nothing y)

handleNewRoute _ =
  pure unit

handleModuleTypeaheadOutput :: TypeaheadOutput Module -> ExplorerAction
handleModuleTypeaheadOutput (SelectedOption module')
  | module' == Just allModules = SelectModule Nothing
  | otherwise = SelectModule module'
