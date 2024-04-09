module App.Components.Editor.Renderer where

import FatPrelude hiding (div)

import App.CSS.ClassNames (formulaBox, formulaBoxContainer, functionSignature, invalidFormula, materialIcons, selectedSuggestionOption, suggestionOption, suggestionsDropdown, unknownFormula, validFormula)
import App.CSS.Ids (formulaBoxId, functionSignatureId, suggestionsDropdownId)
import App.Components.Editor.Models (EditorAction(..), EditorState)
import App.Components.Table.Formula (FormulaState(..))
import App.Editor.Formula (SuggestionTerm)
import App.Utils.KeyCode (mkKeyAction)
import Data.Array ((!!))
import Halogen.HTML (ClassName, HTML, div, i, text)
import Halogen.HTML.Events (onFocusIn, onKeyDown, onKeyUp)
import Halogen.HTML.Properties (class_, classes, id, spellcheck)

render
  :: forall a. EditorState -> HTML a EditorAction
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
        , classes [ formulaBox, formulaStateToClass formulaState ]
        , spellcheck false
        , onKeyDown $ mkKeyAction $ KeyDown
            (suggestions !! unwrap selectedSuggestionId)
        , onKeyUp $ mkKeyAction KeyUp
        , onFocusIn FocusIn
        ]
        []
    , div
        [ id $ show functionSignatureId
        , classes [ functionSignature ]
        ]
        []
    , div
        [ id $ show suggestionsDropdownId
        , class_ suggestionsDropdown
        ]
        (mapWithIndex (renderSuggestion st) suggestions)
    ]

formulaStateToClass :: FormulaState -> ClassName
formulaStateToClass = case _ of
  ValidFormula -> validFormula
  InvalidFormula -> invalidFormula
  UnknownFormula -> unknownFormula

renderSuggestion
  :: forall i. EditorState -> Int -> SuggestionTerm -> HTML i EditorAction
renderSuggestion { selectedSuggestionId } n suggestionTerm = div
  [ classes $ [ suggestionOption ]
      <>? (wrap n == selectedSuggestionId)
      /\ selectedSuggestionOption
  ]
  [ i
      [ class_ materialIcons ]
      [ text "functions" ]
  , text $ show suggestionTerm
  ]
