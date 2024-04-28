module App.Components.Editor.Renderer where

import FatPrelude hiding (div)

import App.CSS.ClassNames (formulaBox, formulaBoxContainer, functionSignature, invalidFormula, selectedSuggestionOption, suggestionOption, suggestionsDropdown, unknownFormula, validFormula)
import App.CSS.Ids (formulaBoxId, functionSignatureId, suggestionsDropdownId)
import App.Components.Editor.Models (EditorAction(..), EditorState)
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Editor.Suggestion (SuggestionTerm)
import App.Utils.HTML (materialIcon)
import App.Utils.KeyCode (mkKeyAction)
import Data.Array ((!!))
import Halogen (AttrName(..))
import Halogen.HTML (ClassName, HTML, attr, div, text)
import Halogen.HTML.Events (onFocusIn, onKeyDown, onKeyUp, onMouseDown, onMouseEnter)
import Halogen.HTML.Properties (class_, classes, id, spellcheck)

render :: forall a. EditorState -> HTML a EditorAction
render
  st@
    { formulaState
    , suggestions
    , selectedSuggestionId
    } =
  div
    [ class_ formulaBoxContainer ]
    [ div
        [ id $ show formulaBoxId
        , attr (AttrName "contentEditable") (show true)
        , classes [ formulaBox, formulaStateToClass formulaState ]
        , spellcheck false
        , onKeyDown $ mkKeyAction $ KeyDown selectedSuggestion
        , onKeyUp $ mkKeyAction KeyUp
        , onMouseDown MouseDown
        , onFocusIn FocusIn
        ]
        []
    , div
        [ id $ show functionSignatureId
        , class_ functionSignature
        ]
        []
    , div
        [ id $ show suggestionsDropdownId
        , class_ suggestionsDropdown
        , onMouseDown $ ClickSuggestion selectedSuggestion
        ]
        (mapWithIndex (renderSuggestion st) suggestions)
    ]
  where
  selectedSuggestion = suggestions !! unwrap selectedSuggestionId

renderSuggestion
  :: forall i. EditorState -> Int -> SuggestionTerm -> HTML i EditorAction
renderSuggestion { selectedSuggestionId } n suggestionTerm =
  div
    [ classes $ [ suggestionOption ]
        <>? (wrap n == selectedSuggestionId)
        /\ selectedSuggestionOption
    , onMouseEnter $ HoverSuggestion $ wrap n
    ]
    [ materialIcon "functions"
    , text $ show suggestionTerm
    ]

formulaStateToClass :: FormulaState -> ClassName
formulaStateToClass = case _ of
  ValidFormula -> validFormula
  InvalidFormula -> invalidFormula
  UnknownFormula -> unknownFormula
