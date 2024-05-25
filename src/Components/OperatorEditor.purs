module App.Components.OperatorEditor where

import FatPrelude hiding (div)

import App.AppStore (Store)
import App.CSS.ClassNames (flexRow, functionEditorModal, functionNameInput, operatorEditorModal)
import App.Components.Typeahead (TypeaheadOutput(..))
import App.Components.Typeahead as Typeahead
import App.Parser.Common as Parser
import App.Routes (Route)
import App.SyntaxTree.Common (Module(..), QVar(..), QVarOp(..), Var(..), VarOp(..))
import App.SyntaxTree.FnDef (Associativity(..), OpInfo, Precedence(..))
import App.Utils.Dom (focusHooksRef, prevent)
import App.Utils.HTML (modal, renderWhen, submitButtons)
import App.Utils.KeyCode (KeyCode(..))
import App.Utils.Set (fromArray) as Set
import Bookhound.Parser (runParser)
import Data.HashMap as HashMap
import Data.Lazy as Lazy
import Halogen (RefLabel(..), Slot)
import Halogen.Component (Component)
import Halogen.HTML (div, input, slot)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (class_, classes, placeholder, value)
import Halogen.Hooks (useQuery, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import Web.UIEvent.KeyboardEvent (ctrlKey)

component
  :: Component OperatorEditorQuery OperatorEditorInput OperatorEditorOutput Aff
component = Hooks.component
  \tokens { route, store } -> Hooks.do

    active /\ setActive <- usePutState false
    initialQVarOp /\ setInitialQVarOp <- usePutState
      $ QVarOp Nothing (VarOp mempty)
    opModule /\ setOpModule <- usePutState $ Module mempty
    operator /\ setOperator <- usePutState $ VarOp mempty
    fnModule /\ setFnModule <- usePutState $ Module mempty
    function /\ setFunction <- usePutState $ Var mempty
    precedence /\ setPrecedence <- usePutState P0
    associativity /\ setAssociativity <- usePutState L

    let
      opEditor = Lazy.defer \_ ->
        div [ classes [ functionEditorModal, operatorEditorModal ] ]
          [ div
              [ class_ flexRow ]
              [ slot _opModuleTypeahead unit Typeahead.component
                  { allOptions: store.modules
                  , initialOption: Just opModule
                  , maxOptions: 5
                  , placeholder: "Module name"
                  }
                  case _ of SelectedOption x -> setOpModule x
              , input
                  [ class_ functionNameInput
                  , value $ show operator
                  , onValueInput
                      $ traverse_ setOperator
                      <<< runParser Parser.varOp
                  , placeholder $ "Operator name"
                  ]
              ]
          , div
              [ class_ flexRow ]
              [ slot _fnModuleTypeahead2 unit Typeahead.component
                  { allOptions: store.modules
                  , initialOption: Just fnModule
                  , maxOptions: 5
                  , placeholder: "Module name"
                  }
                  case _ of SelectedOption x -> setFnModule x
              , slot _functionTypeahead unit Typeahead.component
                  { allOptions: Set.fromArray
                      $ map (case _ of QVar _ x -> x)
                      $ filter (case _ of QVar x _ -> x == Just fnModule)
                      $ HashMap.keys store.fnsMap
                  , initialOption: Just function
                  , maxOptions: 5
                  , placeholder: "Function name"
                  }
                  case _ of SelectedOption x -> setFunction x
              ]
          , div
              [ class_ flexRow ]
              [ slot _associativityTypeahead unit Typeahead.component
                  { allOptions: Set.fromArray [ L, R ]
                  , initialOption: Just associativity
                  , maxOptions: 2
                  , placeholder: "Associativity"
                  }
                  case _ of SelectedOption x -> setAssociativity x

              , slot _precedenceTypeahead unit Typeahead.component
                  { allOptions: Set.fromArray enumValues
                  , initialOption: Just precedence
                  , maxOptions: 5
                  , placeholder: "Precedence"
                  }
                  case _ of SelectedOption x -> setPrecedence x
              ]
          , div [ class_ flexRow ] (submitButtons saveAndExit exit)
          ]

      handleKeyDown keyCode ev
        | keyCode `elem` [ ArrowUp, ArrowDown ] = prevent ev
        | keyCode == Escape = exit
        | ctrlKey ev && keyCode == Enter = saveAndExit
        | otherwise = pure unit

      save =
        Hooks.raise tokens.outputToken
          $ ModifiedOperator
              { initialQVarOp
              , qVarOp: QVarOp (Just opModule) operator
              , opInfo:
                  { id: { opModule, opName: operator }
                  , fnName: QVar (Just fnModule) function
                  , precedence
                  , associativity
                  }
              }

      exit = do
        setActive false
        Hooks.raise tokens.outputToken ClosedOperatorEditor

      saveAndExit = save *> exit

    useQuery tokens.queryToken case _ of
      OpenOperatorEditor { qVarOp: QVarOp x y, opInfo } next -> do
        setInitialQVarOp $ QVarOp x y
        setOpModule $ unsafeFromJust x
        setOperator y
        setFnModule $ unsafeFromJust z
        setFunction t
        setPrecedence opInfo.precedence
        setAssociativity opInfo.associativity
        setActive true
        focusHooksRef modalContainerRef
        pure $ Just next
        where
        { fnName: QVar z t } = opInfo

    Hooks.captures { route } useTickEffect
      (setActive false *> mempty)

    Hooks.pure
      $ renderWhen active
      $ map (modal modalContainerRef handleKeyDown) opEditor

modalContainerRef :: RefLabel
modalContainerRef = RefLabel "modalContainerRef"

_operatorEditor :: Proxy "operatorEditor"
_operatorEditor = Proxy

_opModuleTypeahead :: Proxy "opModuleTypeahead"
_opModuleTypeahead = Proxy

_fnModuleTypeahead2 :: Proxy "fnModuleTypeahead"
_fnModuleTypeahead2 = Proxy

_functionTypeahead :: Proxy "functionTypeahead"
_functionTypeahead = Proxy

_associativityTypeahead :: Proxy "associativityTypeahead"
_associativityTypeahead = Proxy

_precedenceTypeahead :: Proxy "precedenceTypeahead"
_precedenceTypeahead = Proxy

type OperatorEditorInput = { route :: Route, store :: Store }

data OperatorEditorOutput
  = ModifiedOperator
      { initialQVarOp :: QVarOp, qVarOp :: QVarOp, opInfo :: OpInfo }
  | ClosedOperatorEditor

data OperatorEditorQuery a =
  OpenOperatorEditor { qVarOp :: QVarOp, opInfo :: OpInfo } a

type OperatorEditorSlot = Slot OperatorEditorQuery OperatorEditorOutput Unit
