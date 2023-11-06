module App.Utils.Dom where

import FatPrelude hiding (span)

import App.CSS.Ids (ElementId(..), ElementType, cellId, formulaBoxId, formulaSignatureId)
import App.Components.Table.Cell (Cell, showCell)
import App.Components.Table.SyntaxAtom (SyntaxAtom, condenseSyntaxAtoms, fnSigToSyntaxAtoms, syntaxAtomParser, syntaxAtomToClassName)
import App.Evaluator.Common (LocalFormulaCtx, lookupBuiltinFn, lookupModuleFn, lookupOperator)
import App.Parser.Common (qVar, qVarOp)
import App.SyntaxTree.Common (QVar(..), Var(..), preludeModule)
import App.SyntaxTree.FnDef (FnId, FnSig)
import App.Utils.Range as Range
import App.Utils.Selection (Selection)
import App.Utils.Selection as Selection
import App.Utils.String (startsWith) as String
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators as Combinators
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Control.Alternative ((<|>))
import Data.Int as Int
import Data.String (Pattern(..), indexOf', lastIndexOf')
import Data.String.CodePoints (length) as String
import Data.String.CodeUnits (slice) as String
import Data.Unfoldable as Unfoldable
import Halogen.HTML (HTML, span, text)
import Halogen.HTML.Properties (class_)
import Halogen.VDom.DOM.StringRenderer as StringRenderer
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Web.Clipboard (Clipboard, clipboard)
import Web.DOM (Element, Node, ParentNode)
import Web.DOM.Element (getBoundingClientRect, id)
import Web.DOM.Node (firstChild, nextSibling, nodeName, nodeValue, parentNode, setTextContent, textContent)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.Event (Event, preventDefault, target)
import Web.Event.EventTarget (EventTarget)
import Web.HTML (HTMLElement, Window, window)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus, toElement, toNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent (WheelEvent)

performSyntaxHighlight :: forall m. MonadEffect m => m Unit
performSyntaxHighlight = liftEffect do
  formulaBox <- justSelectElementById formulaBoxId
  selection <- getSelection =<< window
  caretPosition <- getCaretPosition selection (toNode formulaBox)
  formulaText <- getFormulaBoxContents
  let
    html = unwrap <$> formulaElements formulaText
    htmlString = fold $ StringRenderer.render (const mempty) <$> html
  emptyFormulaBox
  setInnerHTML (toElement formulaBox) htmlString
  traverse_ (setCaretPosition selection $ toNode formulaBox) caretPosition

displayFunctionType :: forall m. MonadEffect m => LocalFormulaCtx -> m Unit
displayFunctionType ctx = liftEffect do
  formulaBox <- justSelectElementById formulaBoxId
  formulaSignature <- justSelectElementById formulaSignatureId
  selection <- getSelection =<< window
  ancestors <- getAncestorNodes =<< Selection.anchorNode selection

  when (any (refEquals $ toNode formulaBox) ancestors) do
    index <- getCaretPosition selection (toNode formulaBox)
    formulaText <- getFormulaBoxContents
    let
      fnId = fromMaybe { fnModule: preludeModule, fnName: Var mempty }
        (getCurrentFnId ctx formulaText =<< index)
      fnQVar = QVar (Just fnId.fnModule) fnId.fnName

      fnInfo = map unwrap $ hush
        $ flip evalState ctx
        $ runExceptT
        $ lookupModuleFn fnQVar

      builtinFnInfo = hush
        $ flip evalState ctx
        $ runExceptT
        $ lookupBuiltinFn fnId.fnName

      fnSig :: Maybe (FnSig ())
      fnSig = pick <$> builtinFnInfo <|> pick <$> fnInfo

      html = unwrap <$> foldMap (fnSigElements fnId) fnSig
      htmlString = fold
        $ StringRenderer.render (const mempty)
        <$> html
    setInnerHTML (toElement formulaSignature) htmlString

syntaxAtomsToElements :: forall a b. Array SyntaxAtom -> Array (HTML a b)
syntaxAtomsToElements = map toElement <<< condenseSyntaxAtoms
  where
  toElement atom = span
    [ class_ $ syntaxAtomToClassName atom ]
    [ text $ show atom ]

formulaElements :: forall a b. String -> Array (HTML a b)
formulaElements =
  syntaxAtomsToElements <<< fold <<< runParser syntaxAtomParser

fnSigElements :: forall a b r. FnId -> FnSig r -> Array (HTML a b)
fnSigElements id sig =
  syntaxAtomsToElements $ fnSigToSyntaxAtoms id sig

getCurrentFnId :: LocalFormulaCtx -> String -> Int -> Maybe FnId
getCurrentFnId ctx formulaText index =
  qVarToFnId <$> join (hush $ runParser fnParser currentWord)
  where
  fnParser = Just <$> qVar Combinators.<|> lookupOp <$> qVarOp
  lookupOp op = _.fnName
    <$> hush (flip evalState ctx $ runExceptT $ lookupOperator op)
  qVarToFnId (QVar module' var) =
    { fnModule: fromMaybe preludeModule module', fnName: var }

  startIndex = fromMaybe 0 $ map inc
    $ maximum
    $ filterMap (myLastIndexOf' (dec index) formulaText)
    $ (Pattern <$> (separators <> [ "(", "[" ]))

  endIndex = fromMaybe (String.length formulaText)
    $ minimum
    $ filterMap (myIndexOf' index formulaText)
    $ (Pattern <$> (separators <> [ ")", "]" ]))

  currentWord = String.slice startIndex endIndex formulaText
  separators = [ " ", " ", "\n", "\t", "," ]

  myLastIndexOf' n str pattern = lastIndexOf' pattern n str
  myIndexOf' n str pattern = indexOf' pattern n str

getFormulaBoxContents :: forall m. MonadEffect m => m String
getFormulaBoxContents = liftEffect
  (innerText =<< justSelectElementById formulaBoxId)

emptyFormulaBox :: forall m. MonadEffect m => m Unit
emptyFormulaBox = liftEffect
  (setTextContent mempty <<< toNode =<< justSelectElementById formulaBoxId)

emptyFormulaSignature :: forall m. MonadEffect m => m Unit
emptyFormulaSignature = liftEffect
  ( setTextContent mempty <<< toNode
      =<< justSelectElementById formulaSignatureId
  )

parseElements
  :: forall m a
   . MonadEffect m
  => (String -> Maybe a)
  -> Array Element
  -> m (Array a)
parseElements parseFn elems = liftEffect
  (filterMap parseFn <$> traverse id elems)

getVisibleCols :: forall m. MonadEffect m => m (Array Element)
getVisibleCols = selectAllVisibleElements $ QuerySelector "th.column-header"

getVisibleRows :: forall m. MonadEffect m => m (Array Element)
getVisibleRows = selectAllVisibleElements $ QuerySelector "th.row-header"

focusCellElem :: forall m. MonadEffect m => Cell -> Maybe ElementType -> m Unit
focusCellElem cell subElem = actOnCellElem cell focus subElem

focusCell :: forall m. MonadEffect m => Cell -> m Unit
focusCell = flip focusCellElem Nothing

focusById :: forall m. MonadEffect m => ElementId -> m Unit
focusById = flip actOnElementById focus

actOnCellElem
  :: forall m
   . MonadEffect m
  => Cell
  -> (HTMLElement -> Effect Unit)
  -> Maybe ElementType
  -> m Unit
actOnCellElem cell action subElem =
  actOnElementById
    ( ElementId
        (show cellId <> showCell cell <> foldMap ((" " <> _) <<< show) subElem)
    )
    action

actOnElementById
  :: forall m
   . MonadEffect m
  => ElementId
  -> (HTMLElement -> Effect Unit)
  -> m Unit
actOnElementById id action = liftEffect
  (traverse_ action =<< selectElementById id)

selectAllVisibleElements
  :: forall m. MonadEffect m => QuerySelector -> m (Array Element)
selectAllVisibleElements query = liftEffect $ do
  elems <- selectAllElements query
  visibleElems <- sequence $ elemsInViewport <$> (toElement <$$> elems)
  pure $ fold visibleElems

selectAllElements
  :: forall m
   . MonadEffect m
  => QuerySelector
  -> m (Maybe (NonEmptyArray HTMLElement))
selectAllElements query = liftEffect $ do
  nodes <- querySelectorHelper querySelectorAll query
  elems <- NodeList.toArray nodes
  pure $ (traverse HTMLElement.fromNode) =<< fromArray elems

selectElement
  :: forall m. MonadEffect m => QuerySelector -> m (Maybe HTMLElement)
selectElement query = liftEffect $ do
  maybeElem <- querySelectorHelper querySelector query
  pure $ HTMLElement.fromElement =<< maybeElem

justSelectElementById
  :: forall m. MonadEffect m => ElementId -> m HTMLElement
justSelectElementById x =
  unsafeFromJust <$> selectElementById x

selectElementById
  :: forall m. MonadEffect m => ElementId -> m (Maybe HTMLElement)
selectElementById =
  selectElement <<< QuerySelector <<< ("#" <> _) <<< show

querySelectorHelper
  :: forall a
   . (QuerySelector -> ParentNode -> Effect a)
  -> QuerySelector
  -> Effect a
querySelectorHelper function query =
  function query <<< HTMLDocument.toParentNode =<< getDocument

getDocument :: Effect HTMLDocument
getDocument = Window.document =<< window

getClipboard :: forall m. MonadEffect m => m Clipboard
getClipboard = liftEffect
  (clipboard =<< Window.navigator =<< window)

setCaretPosition :: Selection -> Node -> Int -> Effect Unit
setCaretPosition selection parentNode offset = do
  childNode <- unsafeFromJust <$> firstChild parentNode
  anchor <- Selection.anchorNode selection
  (rangeNode /\ rangeOffset) <- go childNode anchor offset
  range <- Range.createCollapsedRange rangeNode rangeOffset
  Selection.resetRange selection range
  where
  go node anchor position = do
    len <- String.length <$> textContent node
    if len >= position then
      (_ /\ position) <$> getChildOrNode node
    else do
      sibling <- unsafeFromJust <$> nextSibling node
      go sibling anchor (position - len)

getCaretPosition :: Selection -> Node -> Effect (Maybe Int)
getCaretPosition selection parentNode = do
  childNode <- unsafeFromJust <$> firstChild parentNode
  join $ go childNode
    <$> Selection.anchorNode selection
    <*> Selection.anchorOffset selection
  where
  go node anchor position = do
    len <- String.length <$> textContent node
    textNode <- getChildOrNode node
    if any (refEquals anchor) [ node, textNode ] then
      pure $ pure position
    else runMaybeT do
      sibling <- MaybeT $ nextSibling node
      MaybeT $ go sibling anchor (position + len)

getChildOrNode :: Node -> Effect Node
getChildOrNode node = do
  child <- firstChild node
  pure $ unsafeFromJust (child <|> pure node)

getAncestorNodes :: Node -> Effect (Array Node)
getAncestorNodes node = do
  parentNode' <- parentNode node
  grandParentNode <- join <$> traverse parentNode parentNode'
  pure $ filterMap identity [ parentNode', grandParentNode ]

getNodeText :: Node -> Effect String
getNodeText node = do
  child <- firstChild node
  childText <- join <$> traverse nodeValue child
  nodeText <- nodeValue node
  pure $ fold (brText <|> nodeText <|> childText)
  where
  brText = toMaybe' (nodeName node == "br") "\n"

elemsInViewport
  :: forall m. MonadEffect m => NonEmptyArray Element -> m (Array Element)
elemsInViewport elems = liftEffect $ do
  w <- window
  wHeight <- Height <<< toNumber <$> Window.innerHeight w
  wWidth <- Width <<< toNumber <$> Window.innerWidth w
  filterA (isInViewport wHeight wWidth) elems

isInViewport
  :: forall m. MonadEffect m => Height -> Width -> Element -> m Boolean
isInViewport (Height wHeight) (Width wWidth) element =
  liftEffect $ do
    rect <- getBoundingClientRect element
    let
      visibleY = rect.top < wHeight && pos rect.bottom
      visibleX = rect.left < wWidth && pos rect.right
    pure $ visibleX && visibleY

withPrevent :: forall m a b. MonadEffect m => IsEvent a => a -> m b -> m b
withPrevent ev next = prevent ev *> next

scrollByX :: Number -> Window -> Effect Unit
scrollByX = flip scrollBy 0.0

scrollByY :: Number -> Window -> Effect Unit
scrollByY = scrollBy 0.0

scrollBy :: Number -> Number -> Window -> Effect Unit
scrollBy x y = Window.scrollBy (unsafeCoerce x) (unsafeCoerce y)

prevent :: forall m a. MonadEffect m => IsEvent a => a -> m Unit
prevent ev = liftEffect (preventDefault $ toEvent ev)

mkKeyAction :: forall a. (KeyCode -> KeyboardEvent -> a) -> KeyboardEvent -> a
mkKeyAction ctor ev = ctor (fetchKeyCode ev) ev
  where
  fetchKeyCode = parseKeyCode <<< KeyboardEvent.code

parseKeyCode :: String -> KeyCode
parseKeyCode "ArrowLeft" = ArrowLeft
parseKeyCode "ArrowRight" = ArrowRight
parseKeyCode "ArrowUp" = ArrowUp
parseKeyCode "ArrowDown" = ArrowDown
parseKeyCode "Enter" = Enter
parseKeyCode "Tab" = Tab
parseKeyCode "Space" = Space
parseKeyCode "Delete" = Delete
parseKeyCode "Backspace" = Delete
parseKeyCode "ShiftLeft" = Shift
parseKeyCode "ShiftRight" = Shift
parseKeyCode "ControlLeft" = Control
parseKeyCode "ControlRight" = Control
parseKeyCode "MetaLeft" = Control
parseKeyCode "MetaRight" = Control
parseKeyCode "Comma" = Comma
parseKeyCode str
  | String.startsWith "Key" str
  , Just ch <- last' $ toCharArray str = CharKeyCode ch
parseKeyCode str
  | String.startsWith "Digit" str
  , Just n <-
      Int.fromString $ fromCharArray $ Unfoldable.fromMaybe $ last' $
        toCharArray str =
      DigitKeyCode n
parseKeyCode str = OtherKeyCode str

toEvent :: forall a. IsEvent a => a -> Event
toEvent = unsafeCoerce

getTarget :: forall a. IsEvent a => a -> Maybe EventTarget
getTarget = target <<< toEvent

toMouseEvent :: forall a. IsEvent a => a -> MouseEvent
toMouseEvent = unsafeCoerce

foreign import innerText :: HTMLElement -> Effect String

foreign import setInnerHTML :: Element -> String -> Effect Unit

foreign import getSelection :: Window -> Effect Selection

foreign import refEquals :: forall a. a -> a -> Boolean

newtype Height = Height Number
newtype Width = Width Number

data KeyCode
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Enter
  | Tab
  | Space
  | Delete
  | Shift
  | Control
  | Comma
  | CharKeyCode Char
  | DigitKeyCode Int
  | OtherKeyCode String

derive instance Eq KeyCode

class ModKeyEvent a where
  shiftKey :: a -> Boolean
  ctrlKey :: a -> Boolean

instance ModKeyEvent KeyboardEvent where
  shiftKey = KeyboardEvent.shiftKey
  ctrlKey ev = KeyboardEvent.ctrlKey ev || KeyboardEvent.metaKey ev

instance ModKeyEvent MouseEvent where
  shiftKey = MouseEvent.shiftKey
  ctrlKey ev = MouseEvent.ctrlKey ev || MouseEvent.metaKey ev

class IsEvent :: forall k. k -> Constraint
class IsEvent a

instance IsEvent FocusEvent
instance IsEvent MouseEvent
instance IsEvent KeyboardEvent
instance IsEvent InputEvent
instance IsEvent DragEvent
instance IsEvent WheelEvent
