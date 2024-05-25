module App.Components.FunctionEditor where

import FatPrelude hiding (div)

import App.AppStore (Store)
import App.CSS.ClassNames (flexRow, formulaBox, functionDocString, functionEditorModal, functionNameInput, functionSignatureInput, validFormula)
import App.Components.Editor (_editor)
import App.Components.Editor as Editor
import App.Components.Editor.Models (EditorQuery(..))
import App.Components.Spreadsheet.Formula (FormulaState(..))
import App.Components.Typeahead (TypeaheadOutput(..))
import App.Components.Typeahead as Typeahead
import App.Parser.Common (var) as Parser
import App.Parser.FnDef (fnBody, fnSignature) as Parser
import App.Printer.Common (printDefault)
import App.Printer.FnDef as Printer
import App.Routes (Route)
import App.SyntaxTree.Common (Module(..), QVar(..), Var(..))
import App.SyntaxTree.FnDef (FnBody(..), FnInfo(..), FnSig, Object(..))
import App.Utils.Dom (focusHooksRef, prevent)
import App.Utils.Event (ctrlKey)
import App.Utils.HTML (modal, renderWhen, submitButtons)
import App.Utils.KeyCode (KeyCode(..))
import Bookhound.Parser (runParser)
import Data.Array as Array
import Data.HashMap as HashMap
import Data.Lazy as Lazy
import Halogen (RefLabel(..), Slot)
import Halogen.Component (Component)
import Halogen.HTML (div, input, slot, textarea)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (class_, classes, placeholder, rows, style, value)
import Halogen.Hooks (useQuery, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import Halogen.Hooks.HookM (tell)

component
  :: Component FunctionEditorQuery FunctionEditorInput FunctionEditorOutput Aff
component = Hooks.component
  \tokens { route, store } -> Hooks.do

    active /\ setActive <- usePutState false
    initialQVar /\ setInitialQVar <- usePutState $ QVar Nothing (Var mempty)
    module' /\ setModule <- usePutState $ Module mempty
    function /\ setFunction <- usePutState $ Var mempty
    params /\ setParams <- usePutState []
    returnType /\ setReturnType <- usePutState Nothing
    docString /\ setDocString <- usePutState mempty
    body /\ setBody <- usePutState $ Object' NullObj

    let
      fnEditor = Lazy.defer \_ ->
        div [ class_ functionEditorModal ]
          [ div
              [ class_ flexRow ]
              [ slot _moduleTypeahead unit Typeahead.component
                  { allOptions: store.modules
                  , initialOption: Just module'
                  , maxOptions: 5
                  , placeholder: "Module name"
                  }
                  case _ of SelectedOption x -> setModule x
              , input
                  [ class_ functionNameInput
                  , value $ show function
                  , onValueInput
                      $ traverse_ setFunction
                      <<< runParser Parser.var
                  , placeholder $ "Function name"
                  ]
              , input
                  [ class_ functionSignatureInput
                  , value
                      $ whenMonoid (not $ Array.null params)
                      $ printDefault
                      $ Printer.fnSignature params returnType
                  , onValueInput
                      $ traverse_ (bitraverse_ setParams setReturnType)
                      <<< runParser Parser.fnSignature
                  , placeholder "Function signature"
                  ]
              ]
          , slot _editor unit Editor.component
              { placeholder: "Function body"
              , formulaState: ValidFormula
              , store
              }
              \_ -> pure unit
          , textarea
              [ classes [ formulaBox, validFormula, functionDocString ]
              , value docString
              , onValueInput setDocString
              , rows 3
              , style "resize: none"
              , placeholder "Function docString"
              ]
          , div [ class_ flexRow ] (submitButtons saveAndExit exit)
          ]

      handleKeyDown keyCode ev
        | keyCode `elem` [ ArrowUp, ArrowDown ] = prevent ev
        | keyCode == Escape = exit
        | ctrlKey ev && keyCode == Enter = saveAndExit
        | otherwise = pure unit

      save = do
        text <- Hooks.request tokens.slotToken _editor unit GetEditorContents
        let fnBody = hush $ runParser Parser.fnBody $ fold text
        Hooks.raise tokens.outputToken
          $ ModifiedFunction
              { initialQVar
              , qVar: QVar (Just module') function
              , fnInfo: FnInfo
                  { id: Just { fnModule: module', fnName: function }
                  , body: fromMaybe body fnBody
                  , params
                  , returnType
                  , doc: docString
                  , scope: zero
                  , argsMap: HashMap.empty
                  }
              }

      exit = do
        setActive false
        Hooks.raise tokens.outputToken ClosedFunctionEditor

      saveAndExit =
        save *> exit

    Hooks.captures { route } useTickEffect
      (setActive false *> mempty)

    useQuery tokens.queryToken case _ of
      OpenFunctionEditor { qVar: QVar x y, fnSig, fnBody } next -> do
        setInitialQVar $ QVar x y
        setModule $ unsafeFromJust x
        setFunction y
        setBody fnBody
        setDocString fnSig.doc
        setParams fnSig.params
        setReturnType fnSig.returnType
        setActive true
        focusHooksRef modalContainerRef
        tell tokens.slotToken _editor unit
          $ UpdateEditorContent
          $ printDefault
          $ Printer.fnBody fnBody
        pure $ Just next

    Hooks.pure
      $ renderWhen active
      $ map (modal modalContainerRef handleKeyDown) fnEditor

modalContainerRef :: RefLabel
modalContainerRef = RefLabel "modalContainerRef"

_functionEditor :: Proxy "functionEditor"
_functionEditor = Proxy

_moduleTypeahead :: Proxy "moduleTypeahead"
_moduleTypeahead = Proxy

type FunctionEditorInput = { route :: Route, store :: Store }

data FunctionEditorOutput
  = ModifiedFunction { initialQVar :: QVar, qVar :: QVar, fnInfo :: FnInfo }
  | ClosedFunctionEditor

data FunctionEditorQuery a =
  OpenFunctionEditor { qVar :: QVar, fnSig :: FnSig, fnBody :: FnBody } a

type FunctionEditorSlot = Slot FunctionEditorQuery FunctionEditorOutput Unit
