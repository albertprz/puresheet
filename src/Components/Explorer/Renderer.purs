module App.Components.Explorer.Renderer where

import FatPrelude hiding (div)

import App.AppStore (mkLocalContext)
import App.CSS.ClassNames (explorerContainer, formulaBox, functionContainer, functionDescription, functionDoc, functionRow, functionsList, invisibleContainer, termTypeLabel, unknownFormula)
import App.Components.Explorer.Models (ExplorerAction(..), ExplorerState)
import App.Editor.Formula (SuggestionTerm(..), fnSigElements, formulaElements, getAllAvailableFns, syntaxAtomsToElements)
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Formula (evalExprInCtx)
import App.Explorer.Formula (SuggestionInfo(..), getSuggestionInfo)
import App.Parser.FnDef (fnBody)
import App.Routes (Route(..))
import App.SyntaxTree.Common (QVar(..))
import App.SyntaxTree.FnDef (FnBody(..))
import App.Utils.SyntaxAtom (SyntaxAtom(..))
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (someSepBy)
import Bookhound.Parsers.Char (comma)
import Data.Array ((!!))
import Data.Array as Array
import Data.Ord.Max (Max(..))
import Data.String (length, stripPrefix) as String
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Data.String.Utils (unsafeRepeat) as String
import Halogen.HTML (HTML, br_, div, span, table, td, text)
import Halogen.HTML.Elements (tr)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, style)

render :: forall w. ExplorerState -> HTML w ExplorerAction
render { route, store, module', selectedRowNumber } =
  div
    [ class_
        if route == ExplorerView then explorerContainer
        else invisibleContainer
    ]
    [ div
        [ class_ functionContainer
        ]
        [ div
            [ classes [ formulaBox, unknownFormula ]
            , style "margin-bottom: 50px; overflow: hidden"
            ]
            ( foldMap (renderDocLine fn maxExampleLen ctx)
                (lines doc)
            )
        ]
    , table
        [ class_ functionsList
        , style "border-collapse: collapse"
        ]
        (mapWithIndex renderFunctionRow infos)
    ]
  where
  maxExampleLen = alaF Max foldMap (findExampleLength) (lines doc)
  doc = selectedSuggestion.fnSig.doc
  fn = selectedSuggestion.fn
  infos = filterMap (getSuggestionInfo ctx)
    $ Array.sort
    $ Array.fromFoldable
    $ getAllAvailableFns module' ctx
  ctx = mkLocalContext store
  selectedSuggestion = unwrap
    $ unsafeFromJust (infos !! selectedRowNumber)

renderFunctionRow
  :: forall w. Int -> SuggestionInfo -> HTML w ExplorerAction
renderFunctionRow n (SuggestionInfo { term, fn, fnSig }) =
  tr [ class_ functionRow, onClick $ const $ ClickFunctionRow n ]
    [ td [ class_ termTypeLabel ]
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
  termType = case term of
    BuiltinFnSuggestion _ -> "builtin fn"
    FnSuggestion _ -> "fn"
    OpSuggestion _ -> "op"
    ModuleSuggestion _ -> mempty

renderDocLine
  :: forall w i. QVar -> Int -> LocalFormulaCtx -> String -> Array (HTML w i)
renderDocLine qVar maxLen ctx docLine =
  case String.stripPrefix (Pattern ">>>") (trim docLine) of
    Just str -> [ br_, text ">>> " ]
      <> formulaElements (showExample qVar maxLen str ctx)
    Nothing -> [ span [ class_ functionDoc ] [ text docLine ], br_ ]

showExample :: QVar -> Int -> String -> LocalFormulaCtx -> String
showExample qVar@(QVar module' fnName) maxLen str ctx =
  exampleStr <> String.unsafeRepeat offset " " <> " -> " <> resultStr
  where
  offset = maxLen - String.length (trim str)
  exampleStr = show fnName <> " " <> wrapParens (trim str)
  resultStr = foldMap show $ hush $ evalExprInCtx ctx' expr
  ctx' = ctx { module' = unsafeFromJust module' }
  expr = FnApply (FnVar qVar) $ fromRight [] $ runParser
    (someSepBy comma fnBody)
    str

findExampleLength :: String -> Int
findExampleLength docLine =
  maybe 0 (String.length <<< trim)
    $ String.stripPrefix (Pattern ">>>") (trim docLine)
