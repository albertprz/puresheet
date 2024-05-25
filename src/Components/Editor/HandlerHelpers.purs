module App.Components.Editor.HandlerHelpers where

import FatPrelude

import App.AppStore (Store, mkLocalContext)
import App.Components.Editor.Models (EditorAction(..), EditorState)
import App.Components.Editor.Renderer (formulaBoxRef, functionSignatureRef, suggestionsDropdownRef)
import App.Editor.Suggestion (SuggestionTerm, extractSuggestionFn, getFnAtIndex, getFnSigAndBody, getSuggestionsAtIndex, getTermAtIndex, getWordAtIndex)
import App.Evaluator.Common (LocalFormulaCtx)
import App.SyntaxTree.Common (QVar)
import App.SyntaxTree.FnDef (FnSig)
import App.Utils.Common (refEquals)
import App.Utils.Dom (emptyContents, getAncestorNodes, justGetHTMLElementRef, setInnerHTML, setStyle)
import App.Utils.HTML (fnSigElements, formulaElements)
import App.Utils.Monoid (whenPlus)
import App.Utils.Range as Range
import App.Utils.Selection (getCaretPosition, getSelection, innerText, setCaretPosition)
import App.Utils.Selection as Selection
import Data.Array ((!!))
import Data.String (null, splitAt) as String
import Data.String.CodeUnits (length) as String
import Halogen (HalogenM, subscribe)
import Halogen.Query.Event (eventListener)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toElement, toNode)
import Web.HTML.Window as Window

performSyntaxHighlight
  :: forall a slots o m
   . MonadEffect m
  => HalogenM EditorState a slots o m Unit
performSyntaxHighlight = do
  formulaBox <- toNode <$> justGetHTMLElementRef formulaBoxRef
  selection <- liftEffect $ getSelection =<< window
  formulaText <- getEditorContent
  caretPosition <- getCaretPosition selection formulaBox
  updateEditorContent formulaText
  traverse_ (setCaretPosition selection formulaBox) caretPosition

performAutoComplete
  :: forall a slots o m
   . MonadEffect m
  => SuggestionTerm
  -> HalogenM EditorState a slots o m Unit
performAutoComplete suggestion = do
  formulaBox <- toNode <$> justGetHTMLElementRef formulaBoxRef
  selection <- liftEffect $ getSelection =<< window
  formulaText <- getEditorContent
  caretPosition <- unsafeFromJust <$> getCaretPosition selection formulaBox
  let
    { currentWord, startIndex, endIndex } = getWordAtIndex [ "." ] formulaText
      caretPosition
    offset = String.length suggestionText - String.length currentWord
    newFormulaText = (String.splitAt startIndex formulaText).before
      <> suggestionText
      <> (String.splitAt endIndex formulaText).after
  updateEditorContent newFormulaText
  setCaretPosition selection formulaBox (caretPosition + offset)
  where
  suggestionText = show suggestion

displayFnSig
  :: forall a slots o m
   . MonadEffect m
  => EditorState
  -> Store
  -> HalogenM EditorState a slots o m Unit
displayFnSig st store = do
  formulaBox <- toNode <$> justGetHTMLElementRef formulaBoxRef
  formulaText <- getEditorContent
  selection <- liftEffect $ getSelection =<< window
  idx <- getCaretPosition selection formulaBox
  ancestors <- liftEffect $ getAncestorNodes =<< Selection.anchorNode selection
  let
    ctx = mkLocalContext store
    suggestion = (st.suggestions !! unwrap st.selectedSuggestionId)
      <|> (getFnAtIndex formulaText =<< idx)
    fn = extractSuggestionFn ctx =<< suggestion
    fnSig = whenPlus (any (refEquals formulaBox) ancestors)
      ((map fst <<< (_ `getFnSigAndBody` ctx)) =<< fn)
  when (String.null (trim formulaText) || isJust idx)
    $ maybe emptyFnSig (uncurry setFnSig) (bisequence (fn /\ fnSig))

displayFnSuggestions
  :: forall a slots o m
   . MonadEffect m
  => Store
  -> HalogenM EditorState a slots o m Unit
displayFnSuggestions store = do
  st <- get
  rect <- liftEffect $ Range.getBoundingClientRect
    =<< Selection.getFirstRange
    =<< getSelection
    =<< window
  suggestionsDropdown <- toElement <$>
    justGetHTMLElementRef suggestionsDropdownRef
  let ctx = mkLocalContext store
  suggestions <- getFnSuggestions ctx
  formulaText <- getEditorContent
  when
    ( String.null (trim formulaText) ||
        any (_ /= st.suggestions) suggestions
    )
    do
      liftEffect $ setStyle suggestionsDropdown
        [ "top" /\ (show (rect.top + rect.height) <> "px")
        , "left" /\ (show rect.left <> "px")
        ]
      modify_ _
        { suggestions = fold suggestions
        , selectedSuggestionId = zero
        }

getFnSuggestions
  :: forall st a slots o m
   . MonadEffect m
  => LocalFormulaCtx
  -> HalogenM st a slots o m (Maybe (Array SuggestionTerm))
getFnSuggestions ctx = do
  formulaBox <- toNode <$> justGetHTMLElementRef formulaBoxRef
  formulaText <- getEditorContent
  liftEffect do
    selection <- getSelection =<< window
    ancestors <- getAncestorNodes =<< Selection.anchorNode selection
    idx <- getCaretPosition selection formulaBox
    pure
      $ whenMonoid (any (refEquals formulaBox) ancestors)
      $ map (getSuggestionsAtIndex ctx formulaText) idx

getTermAtCaret
  :: forall a slots o m
   . MonadEffect m
  => HalogenM EditorState a slots o m (Maybe SuggestionTerm)
getTermAtCaret = do
  ctx <- mkLocalContext <$> gets _.store
  formulaBox <- toNode <$> justGetHTMLElementRef formulaBoxRef
  formulaText <- getEditorContent
  selection <- liftEffect $ getSelection =<< window
  caretPosition <- getCaretPosition selection formulaBox
  pure $ getTermAtIndex ctx formulaText =<< caretPosition

setFnSig
  :: forall s a slots o m
   . MonadEffect m
  => QVar
  -> FnSig
  -> HalogenM s a slots o m Unit
setFnSig fn fnSig = do
  functionSignatureDisplay <- justGetHTMLElementRef functionSignatureRef
  liftEffect $ setInnerHTML (toElement functionSignatureDisplay)
    (fnSigElements fn fnSig)

insertEditorNewLine
  :: forall a slots o m
   . MonadEffect m
  => HalogenM EditorState a slots o m Unit
insertEditorNewLine = do
  formulaBox <- justGetHTMLElementRef formulaBoxRef
  formulaText <- getEditorContent
  selection <- liftEffect $ getSelection =<< window
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

setEditorContent
  :: forall s a slots o m
   . MonadEffect m
  => String
  -> HalogenM s a slots o m Unit
setEditorContent formulaText = do
  formulaBox <- justGetHTMLElementRef formulaBoxRef
  liftEffect $ setInnerHTML (toElement formulaBox) (formulaElements formulaText)

updateEditorContent
  :: forall a slots o m
   . MonadEffect m
  => String
  -> HalogenM EditorState a slots o m Unit
updateEditorContent formulaText = do
  emptyEditor
  setEditorContent formulaText
  when (String.null formulaText) do
    emptyFnSig
    modify_ _ { suggestions = [] }

emptyEditor
  :: forall s a slots o m. MonadEffect m => HalogenM s a slots o m Unit
emptyEditor = emptyContents formulaBoxRef

emptyFnSig
  :: forall s a slots o m. MonadEffect m => HalogenM s a slots o m Unit
emptyFnSig = emptyContents functionSignatureRef

emptyFnSuggestions
  :: forall s a slots o m. MonadEffect m => HalogenM s a slots o m Unit
emptyFnSuggestions = emptyContents suggestionsDropdownRef

getEditorContent
  :: forall s a slots o m. MonadEffect m => HalogenM s a slots o m String
getEditorContent = do
  formulaBox <- justGetHTMLElementRef formulaBoxRef
  liftEffect $ innerText formulaBox

subscribeSelectionChange
  :: forall slots s o m
   . MonadEffect m
  => HalogenM s EditorAction slots o m Unit
subscribeSelectionChange = do
  doc <- liftEffect $ Window.document =<< window
  void $ subscribe $ eventListener
    (EventType "selectionchange")
    (HTMLDocument.toEventTarget doc)
    (const $ Just SelectionChange)
