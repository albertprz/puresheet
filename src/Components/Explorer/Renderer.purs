module App.Components.Explorer.Renderer where

import FatPrelude hiding (div)

import App.AppM (AppM)
import App.AppStore (mkLocalContext)
import App.CSS.ClassNames (addTermContainer, deleteFunctionIcon, editFunctionIcon, explorerContainer, flexColumn, flexRow, formulaBox, functionContainer, functionDescription, functionDoc, functionFiltersContainer, functionRow, functionsList, hiddenContainer, invisibleContainer, termTypeLabel, validFormula)
import App.CSS.Ids (functionRowId)
import App.Components.Explorer.FunctionFilter (FnFilter(..), countFnSigParams, evalExample, parseFnFilter, termPredicate)
import App.Components.Explorer.Handler (handleFunctionEditorOutput, handleModuleTypeaheadOutput, handleOperatorEditorOutput)
import App.Components.Explorer.Models (ExplorerAction(..), ExplorerState, Slots, _moduleTypeahead, allModules, functionFilterInputRef)
import App.Components.FunctionEditor (_functionEditor)
import App.Components.FunctionEditor as FunctionEditor
import App.Components.OperatorEditor (_operatorEditor)
import App.Components.OperatorEditor as OperatorEditor
import App.Components.Typeahead as Typeahead
import App.Editor.Suggestion (SuggestionInfo(..), SuggestionTerm(..), getAllAvailableFns, getSuggestionInfo)
import App.Evaluator.Common (LocalFormulaCtx)
import App.Parser.FnDef (fnBody)
import App.Routes (Route(..))
import App.SyntaxTree.Common (QVar(..), preludeModule)
import App.Utils.HTML (addButton, fnSigElements, formulaElements, materialIcon, searchInput, syntaxAtomsToElements)
import App.Utils.KeyCode (mkKeyAction)
import App.Utils.SyntaxAtom (SyntaxAtom(..))
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (someSepBy)
import Bookhound.Parsers.Char (comma)
import CSSPrelude (ComponentHTML)
import Data.Array ((!!))
import Data.Array as Array
import Data.Ord.Max (Max(..))
import Data.Set as Set
import Data.String (length, stripPrefix) as String
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Data.String.Utils (unsafeRepeat) as String
import Halogen (hoist)
import Halogen.HTML (HTML, br_, div, slot, span, table, td, text, tr)
import Halogen.HTML.Events (onClick, onKeyDown, onValueInput)
import Halogen.HTML.Properties (class_, classes, id, placeholder, ref, style, tabIndex, value)
import Halogen.Portal (portalAff)

render :: ExplorerState -> ComponentHTML ExplorerAction Slots AppM
render
  { route, store, module', fnFilter, fnFilterText, selectedRow } =
  div
    [ class_
        case route of
          ExplorerView _ -> explorerContainer
          _ -> invisibleContainer
    , onKeyDown $ mkKeyAction KeyDown

    ]
    [ div
        [ class_ functionContainer ]
        [ div
            [ classes [ formulaBox, validFormula ] ]
            ( case fn of
                Just qVar -> foldMap (renderDocLine qVar maxExampleLen ctx)
                  (lines doc)
                Nothing -> mempty
            )
        , div [ classes [ functionFiltersContainer, flexRow ] ]
            [ slot _moduleTypeahead unit (hoist liftAff Typeahead.component)
                { allOptions: Set.insert allModules store.modules
                , initialOption: Just preludeModule
                , maxOptions: 5
                , placeholder: "Module name"
                }
                handleModuleTypeaheadOutput
            , searchInput
                [ value fnFilterText
                , ref functionFilterInputRef
                , onValueInput $ UpdateFunctionFilter <<< parseFnFilter
                , onKeyDown $ mkKeyAction FunctionFilterKeyDown
                , placeholder "Function name, signature or example"
                ]

            , div [ classes [ addTermContainer, flexColumn ] ]
                [ addButton "Function" [ onClick \_ -> CreateFunction ]
                , addButton "Operator" [ onClick \_ -> CreateOperator ]
                ]
            ]
        ]
    , table
        [ class_ functionsList
        , style "border-collapse: collapse"
        , onKeyDown $ mkKeyAction TableKeyDown
        ]
        (mapWithIndex renderFunctionRow sortedInfos)
    , portalAff _functionEditor unit FunctionEditor.component { route, store }
        Nothing
        handleFunctionEditorOutput
    , portalAff _operatorEditor unit OperatorEditor.component { route, store }
        Nothing
        handleOperatorEditorOutput
    ]
  where
  maxExampleLen = alaF Max foldMap findExampleLength $ lines doc
  doc = foldMap _.fnSig.doc selectedSuggestion
  fn = map _.fn selectedSuggestion
  sortedInfos
    | Just (FnSignature _ _) <- fnFilter = Array.sortWith
        (countFnSigParams <<< _.fnSig <<< unwrap)
        infos
    | otherwise = infos
  infos = filter (termPredicate ctx fnFilter module')
    $ filterMap (getSuggestionInfo ctx)
    $ Array.fromFoldable
    $ getAllAvailableFns module' ctx
  ctx = mkLocalContext store
  selectedSuggestion = unwrap <$> (infos !! selectedRow)

renderFunctionRow
  :: forall w. Int -> SuggestionInfo -> HTML w ExplorerAction
renderFunctionRow n info@(SuggestionInfo { term, fn, fnSig }) =
  tr
    [ id (show $ functionRowId n)
    , tabIndex zero
    , class_ functionRow
    , onClick $ const $ ClickFunctionRow n
    , onKeyDown $ mkKeyAction $ RowKeyDown info
    ]
    [ td
        [ classes $ [ editFunctionIcon ]
            <>? (isBuiltin /\ hiddenContainer)
        , onClick $ ClickEditRow n info
        ]
        [ materialIcon "edit" ]
    , td
        [ classes $ [ deleteFunctionIcon ]
            <>? (isBuiltin /\ hiddenContainer)
        , onClick \_ -> ClickDeleteRow n info
        ]
        [ materialIcon "delete" ]
    , td
        [ class_ termTypeLabel ]
        [ text termType ]
    , td [ class_ functionDescription ] fnNameElems
    , td [ class_ functionDescription ] fnSigElems
    ]
  where
  fnNameElems = syntaxAtomsToElements [ fnSyntaxAtom ]
  fnSigElems = unsafeFromJust
    $ Array.tail
    $ fnSigElements fn fnSig
  fnSyntaxAtom = case term of
    OpSuggestion _ -> Operator $ show term
    _ -> Function $ show term
  isBuiltin = case term of
    BuiltinFnSuggestion _ -> true
    _ -> false

  termType = case term of
    BuiltinFnSuggestion _ -> "builtin fn"
    FnSuggestion _ -> "fn"
    OpSuggestion _ -> "op"
    ModuleSuggestion _ -> mempty

renderDocLine
  :: forall w i. QVar -> Int -> LocalFormulaCtx -> String -> Array (HTML w i)
renderDocLine qVar maxLen ctx docLine =
  case String.stripPrefix (Pattern commentPrefix) (trim docLine) of
    Just str -> [ br_, text (commentPrefix <> " ") ]
      <> formulaElements (showExample qVar maxLen str ctx)
    Nothing -> [ span [ class_ functionDoc ] [ text docLine ], br_ ]

showExample :: QVar -> Int -> String -> LocalFormulaCtx -> String
showExample qVar@(QVar _ fnName) maxLen str ctx =
  exampleStr <> String.unsafeRepeat offset " " <> " = " <> resultStr
  where
  offset = maxLen - String.length (trim str)
  exampleStr = show fnName <> " " <> wrapParens (trim str)
  resultStr = foldMap show $ evalExample qVar args ctx
  args = fromRight [] $ runParser (someSepBy comma fnBody) str

findExampleLength :: String -> Int
findExampleLength docLine =
  maybe 0 (String.length <<< trim)
    $ String.stripPrefix (Pattern commentPrefix) (trim docLine)

commentPrefix :: String
commentPrefix = ">>>"
