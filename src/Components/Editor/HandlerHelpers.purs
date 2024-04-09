module App.Components.Editor.HandlerHelpers where

import FatPrelude

import App.CSS.Ids (formulaBoxId, functionSignatureId, suggestionsDropdownId)
import App.Components.AppStore (Store, mkLocalContext)
import App.Components.Editor.Models (EditorAction(..), EditorState)
import App.Editor.Formula (SuggestionTerm, extractSuggestionFn, fnSigElements, formulaElements, getFnAtIndex, getFnSig, getSuggestionsAtIndex, getWordAtIndex)
import App.Evaluator.Common (LocalFormulaCtx)
import App.SyntaxTree.Common (QVar)
import App.SyntaxTree.FnDef (SimpleFnSig)
import App.Utils.Common (refEquals)
import App.Utils.Dom (emptyContents, getAncestorNodes, justSelectElementById, setInnerHTML, setStyle)
import App.Utils.Monoid (whenPlus)
import App.Utils.Range as Range
import App.Utils.Selection (getCaretPosition, getSelection, innerText, setCaretPosition)
import App.Utils.Selection as Selection
import Data.Array ((!!))
import Data.String (null, splitAt) as String
import Data.String.CodeUnits (length) as String
import Halogen (HalogenM, subscribe)
import Halogen.Query.Event (eventListener)
import Halogen.Store.Monad (class MonadStore, getStore)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toElement, toNode)
import Web.HTML.Window as Window

performSyntaxHighlight :: forall m. MonadEffect m => m Unit
performSyntaxHighlight = liftEffect do
  formulaBox <- toNode <$> justSelectElementById formulaBoxId
  selection <- getSelection =<< window
  formulaText <- getEditorContent
  caretPosition <- getCaretPosition selection formulaBox
  updateEditorContent formulaText
  traverse_ (setCaretPosition selection formulaBox) caretPosition

performAutoComplete :: forall m. MonadEffect m => String -> m Unit
performAutoComplete suggestion = liftEffect do
  formulaBox <- toNode <$> justSelectElementById formulaBoxId
  selection <- getSelection =<< window
  formulaText <- getEditorContent
  caretPosition <- unsafeFromJust <$> getCaretPosition selection formulaBox
  let
    { currentWord, startIndex, endIndex } = getWordAtIndex formulaText
      caretPosition
    offset = String.length suggestion - String.length currentWord
    newFormulaText = (String.splitAt startIndex formulaText).before
      <> suggestion
      <> (String.splitAt endIndex formulaText).after
  updateEditorContent newFormulaText
  setCaretPosition selection formulaBox (caretPosition + offset)

displayFnSig
  :: forall m
   . MonadEffect m
  => EditorState
  -> Store
  -> m Unit
displayFnSig st store = liftEffect do
  formulaBox <- toNode <$> justSelectElementById formulaBoxId
  selection <- getSelection =<< window
  ancestors <- getAncestorNodes =<< Selection.anchorNode selection
  formulaText <- getEditorContent
  idx <- getCaretPosition selection formulaBox
  let
    ctx = mkLocalContext store
    suggestion = (st.suggestions !! unwrap st.selectedSuggestionId)
      <|> (getFnAtIndex formulaText =<< idx)
    fn = extractSuggestionFn ctx =<< suggestion
    fnSig = whenPlus (any (refEquals formulaBox) ancestors)
      ((_ `getFnSig` ctx) =<< fn)
  maybe emptyFnSig (uncurry setFnSig) (bisequence (fn /\ fnSig))

displayFnSuggestions
  :: forall a m
   . MonadEffect m
  => MonadState EditorState m
  => MonadStore a Store m
  => m Unit
displayFnSuggestions = do
  st <- get
  rect <- liftEffect $ Range.getBoundingClientRect
    =<< Selection.getFirstRange
    =<< getSelection
    =<< window
  suggestionsDropdown <- toElement
    <$> justSelectElementById suggestionsDropdownId
  liftEffect $ setStyle suggestionsDropdown
    [ "top" /\ (show (rect.top + rect.height) <> "px")
    , "left" /\ (show rect.left <> "px")
    ]
  store <- getStore
  suggestions <- getFnSuggestions $ mkLocalContext store
  when (suggestions /= st.suggestions)
    ( modify_ _
        { suggestions = suggestions
        , selectedSuggestionId = zero
        }
    )

getFnSuggestions
  :: forall m. MonadEffect m => LocalFormulaCtx -> m (Array SuggestionTerm)
getFnSuggestions ctx = liftEffect do
  selection <- getSelection =<< window
  ancestors <- getAncestorNodes =<< Selection.anchorNode selection
  formulaBox <- toNode <$> justSelectElementById formulaBoxId
  formulaText <- getEditorContent
  idx <- getCaretPosition selection formulaBox
  pure
    $ whenMonoid (any (refEquals formulaBox) ancestors)
    $ foldMap (getSuggestionsAtIndex ctx formulaText) idx

setFnSig :: forall m. MonadEffect m => QVar -> SimpleFnSig -> m Unit
setFnSig fn fnSig = liftEffect do
  formulaSignatureDisplay <- justSelectElementById functionSignatureId
  setInnerHTML (toElement formulaSignatureDisplay) (fnSigElements fn fnSig)

insertEditorNewLine :: forall m. MonadEffect m => m Unit
insertEditorNewLine = liftEffect do
  formulaBox <- justSelectElementById formulaBoxId
  formulaText <- getEditorContent
  selection <- getSelection =<< window
  caretPosition <- getCaretPosition selection (toNode formulaBox)
  let
    { before: formulaBegin, after: formulaEnd } =
      String.splitAt (fromMaybe zero caretPosition) formulaText
    newFormulaText
      | String.null formulaEnd = formulaBegin <> "\n\n"
      | otherwise = formulaBegin <> "\n" <> formulaEnd
  updateEditorContent newFormulaText
  traverse_ (setCaretPosition selection $ toNode formulaBox)
    (inc <$> caretPosition)

setEditorContent :: forall m. MonadEffect m => String -> m Unit
setEditorContent formulaText = do
  formulaBox <- justSelectElementById formulaBoxId
  liftEffect $ setInnerHTML (toElement formulaBox) (formulaElements formulaText)

updateEditorContent :: forall m. MonadEffect m => String -> m Unit
updateEditorContent formulaText = do
  emptyEditor
  setEditorContent formulaText
  when (String.null formulaText) emptyFnSig

emptyEditor :: forall m. MonadEffect m => m Unit
emptyEditor = emptyContents formulaBoxId

emptyFnSig :: forall m. MonadEffect m => m Unit
emptyFnSig = emptyContents functionSignatureId

emptyFnSuggestions :: forall m. MonadEffect m => m Unit
emptyFnSuggestions = emptyContents suggestionsDropdownId

getEditorContent :: forall m. MonadEffect m => m String
getEditorContent = liftEffect
  (innerText =<< justSelectElementById formulaBoxId)

subscribeSelectionChange
  :: forall slots st o m
   . MonadEffect m
  => HalogenM st EditorAction slots o m Unit
subscribeSelectionChange = do
  doc <- liftEffect $ Window.document =<< window
  void $ subscribe $ eventListener
    (EventType "selectionchange")
    (HTMLDocument.toEventTarget doc)
    (const $ Just SelectionChange)
