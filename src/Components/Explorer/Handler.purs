module App.Components.Explorer.Handler where

import FatPrelude

import App.AppM (AppM)
import App.CSS.Ids (functionRowId)
import App.Components.Explorer.FunctionFilter (FnFilter(..))
import App.Components.Explorer.Models (ExplorerAction(..), ExplorerState, Slots, _moduleTypeahead, allModules, functionFilterInputRef)
import App.Components.FunctionEditor (FunctionEditorOutput(..), FunctionEditorQuery(..), _functionEditor)
import App.Components.OperatorEditor (OperatorEditorOutput(..), OperatorEditorQuery(..), _operatorEditor)
import App.Components.Typeahead (TypeaheadOutput(..), TypeaheadQuery(..))
import App.Editor.Suggestion (SuggestionInfo(..), SuggestionTerm(..))
import App.Routes (Route(..))
import App.SyntaxTree.Common (Module(..), QVar(..), QVarOp(..), Var(..), VarOp(..), preludeModule)
import App.SyntaxTree.FnDef (Associativity(..), FnBody(..), Object(..), Precedence(..))
import App.Utils.Dom (focusById, focusRef, scrollById, selectElementById, stopPropagation, withPrevent)
import App.Utils.Event (class IsEvent, ctrlKey, shiftKey)
import App.Utils.KeyCode (KeyCode(..))
import Data.HashMap as HashMap
import Halogen (HalogenM)
import Halogen.Query (tell)
import Halogen.Store.Monad (updateStore)

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
      $ tell _moduleTypeahead unit ActivateTypeahead

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

handleAction (ClickEditRow n info ev) = do
  modify_ _ { selectedRow = n }
  activateEditorModal info ev

handleAction (ClickDeleteRow n (SuggestionInfo { term })) = do
  case term of
    FnSuggestion qVar ->
      updateStore \store -> store
        { fnsMap = HashMap.delete qVar store.fnsMap }
    OpSuggestion qVarOp ->
      updateStore \store -> store
        { operatorsMap = HashMap.delete qVarOp store.operatorsMap }
    _ -> pure unit
  modify_ _ { selectedRow = n }

handleAction (ModifyFunction { initialQVar, qVar, fnInfo }) = do
  updateStore \store -> store
    { fnsMap = HashMap.insert qVar fnInfo
        $ HashMap.delete initialQVar store.fnsMap
    }

handleAction (ModifyOperator { initialQVarOp, qVarOp, opInfo }) = do
  updateStore \store -> store
    { operatorsMap = HashMap.insert qVarOp opInfo
        $ HashMap.delete initialQVarOp store.operatorsMap
    }

handleAction CreateFunction =
  tell _functionEditor unit $ OpenFunctionEditor { qVar, fnSig, fnBody }
  where
  qVar = QVar (Just $ Module []) $ Var mempty
  fnSig = { params: [], returnType: Nothing, doc: mempty }
  fnBody = Object' NullObj

handleAction CreateOperator =
  tell _operatorEditor unit $ OpenOperatorEditor { qVarOp, opInfo }
  where
  qVarOp = QVarOp (Just $ Module []) $ VarOp mempty
  opInfo =
    { id: { opModule: Module [], opName: VarOp mempty }
    , fnName: QVar (Just $ Module []) $ Var mempty
    , precedence: P12
    , associativity: L
    }

handleAction (RowKeyDown info Enter ev) =
  activateEditorModal info ev

handleAction (RowKeyDown info Delete _) = do
  { selectedRow } <- get
  handleAction (ClickDeleteRow selectedRow info)

handleAction (RowKeyDown _ _ _) =
  pure unit

handleAction ClosedModal =
  refreshFocus

activateEditorModal
  :: forall ev st a
   . IsEvent ev
  => SuggestionInfo
  -> ev
  -> HalogenM st a Slots Unit AppM Unit
activateEditorModal (SuggestionInfo { term, fnSig, fnBody, opInfo }) ev = do
  stopPropagation ev
  case term of
    FnSuggestion qVar -> tell _functionEditor unit
      $ OpenFunctionEditor { qVar, fnSig, fnBody }
    OpSuggestion qVarOp -> tell _operatorEditor unit
      $ OpenOperatorEditor { qVarOp, opInfo: unsafeFromJust opInfo }
    _ -> pure unit

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
  tell _moduleTypeahead unit $ SelectOption $ fromMaybe preludeModule module'
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
  | module' == allModules = SelectModule Nothing
  | otherwise = SelectModule $ Just module'

handleFunctionEditorOutput :: FunctionEditorOutput -> ExplorerAction
handleFunctionEditorOutput = case _ of
  ModifiedFunction x -> ModifyFunction x
  ClosedFunctionEditor -> ClosedModal

handleOperatorEditorOutput :: OperatorEditorOutput -> ExplorerAction
handleOperatorEditorOutput = case _ of
  ModifiedOperator x -> ModifyOperator x
  ClosedOperatorEditor -> ClosedModal
