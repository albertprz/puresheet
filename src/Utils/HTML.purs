module App.Utils.HTML where

import FatPrelude hiding (div)

import App.CSS.ClassNames (cellSyntax, functionSyntax, keywordSyntax, materialIcons, modalContainer, modalInnerContainer, moduleSyntax, numberSyntax, operatorSyntax, regularSyntax, stringSyntax, submitButton, symbolSyntax)
import App.CSS.ClassNames as ClassNames
import App.SyntaxTree.Common (QVar)
import App.SyntaxTree.FnDef (FnSig)
import App.Utils.Dom (prevent)
import App.Utils.KeyCode (KeyCode, mkKeyAction)
import App.Utils.SyntaxAtom (SyntaxAtom(..), condenseSyntaxAtoms, fnSigToSyntaxAtoms, syntaxAtomParser)
import Bookhound.Parser (runParser)
import CSSPrelude (searchInputContainer)
import DOM.HTML.Indexed (HTMLbutton, HTMLinput)
import Data.Array as Array
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Halogen (RefLabel)
import Halogen.HTML (ClassName, HTML, IProp, button, div, i, input, span, text)
import Halogen.HTML.Events (onClick, onKeyDown, onWheel)
import Halogen.HTML.Properties (class_, ref, tabIndex)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

renderWhen :: forall w i. Boolean -> Lazy (HTML w i) -> HTML w i
renderWhen cond elem =
  if cond then Lazy.force elem else text mempty

searchInput :: forall w i. Array (IProp HTMLinput i) -> HTML w i
searchInput props = div
  [ class_ searchInputContainer ]
  [ materialIcon "search"
  , input (Array.cons (class_ ClassNames.searchInput) props)
  ]

addButton :: forall w i. String -> Array (IProp HTMLbutton i) -> HTML w i
addButton label props = button
  (Array.cons (class_ ClassNames.addButton) props)
  [ materialIcon "add_box"
  , text label
  ]

submitButtons :: forall w i. i -> i -> Array (HTML w i)
submitButtons onSave onCancel =
  [ button [ class_ submitButton, onClick \_ -> onSave ] [ text "Save" ]
  , button [ class_ submitButton, onClick \_ -> onCancel ] [ text "Cancel" ]
  ]

modal
  :: forall w m
   . MonadEffect m
  => RefLabel
  -> (KeyCode -> KeyboardEvent -> m Unit)
  -> HTML w (m Unit)
  -> HTML w (m Unit)
modal modalContainerRef handleKeyDown child = div
  [ ref modalContainerRef
  , class_ modalContainer
  , tabIndex zero
  , onWheel prevent
  , onKeyDown $ mkKeyAction handleKeyDown
  ]
  [ div [ class_ modalInnerContainer ] [ child ] ]

materialIcon :: forall w i. String -> HTML w i
materialIcon iconName = i
  [ class_ materialIcons ]
  [ text iconName ]

formulaElements :: forall a b. String -> Array (HTML a b)
formulaElements =
  syntaxAtomsToElements <<< fold <<< runParser syntaxAtomParser

fnSigElements :: forall a b. QVar -> FnSig -> Array (HTML a b)
fnSigElements =
  syntaxAtomsToElements <.. fnSigToSyntaxAtoms

syntaxAtomsToElements :: forall a b. Array SyntaxAtom -> Array (HTML a b)
syntaxAtomsToElements = map toElement <<< condenseSyntaxAtoms
  where
  toElement atom = span
    [ class_ $ syntaxAtomToClassName atom ]
    [ text $ show atom ]

syntaxAtomToClassName :: SyntaxAtom -> ClassName
syntaxAtomToClassName = case _ of
  Cell' _ -> cellSyntax
  Number' _ -> numberSyntax
  String' _ -> stringSyntax
  Char' _ -> stringSyntax
  Keyword _ -> keywordSyntax
  Symbol _ -> symbolSyntax
  Operator _ -> operatorSyntax
  Function _ -> functionSyntax
  Module _ -> moduleSyntax
  OtherText _ -> regularSyntax
