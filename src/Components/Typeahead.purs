module App.Components.Typeahead where

import FatPrelude hiding (div)

import App.AppM (AppM)
import App.CSS.ClassNames (selectedTypeaheadOption, typeahead, typeaheadButton, typeaheadOption)
import App.Utils.Bounded (clampArrayIndex)
import App.Utils.Dom (focusHooksRef, withPrevent)
import App.Utils.Event (getTarget, shiftKey)
import App.Utils.HTML (searchInput)
import App.Utils.KeyCode (KeyCode(..), mkKeyAction)
import Data.Array ((!!))
import Data.Array as Array
import Data.String.Utils as String
import Halogen (Component, RefLabel(..), Slot)
import Halogen.HTML (button, div, text)
import Halogen.HTML.Events (onClick, onFocusOut, onKeyDown, onMouseEnter, onValueInput)
import Halogen.HTML.Properties (class_, classes, placeholder, ref, style, tabIndex)
import Halogen.Hooks (useQuery, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useModifyState_, usePutState)
import Web.DOM.Node as Node
import Web.UIEvent.FocusEvent (relatedTarget)

component
  :: forall a
   . Show a
  => Component TypeaheadQuery (TypeaheadInput a) (TypeaheadOutput a) AppM
component = Hooks.component
  \({ outputToken, queryToken })
   { allOptions, initialOption, maxOptions, placeholderText } -> Hooks.do

    chosenOption /\ putOption <- usePutState initialOption
    optionText /\ putOptionText <- usePutState mempty
    isActive /\ modifyActive <- useModifyState_ false
    selectedOptionId /\ putSelectedOptionId <- usePutState zero

    Hooks.captures { isActive } useTickEffect do
      when isActive $ focusHooksRef searchInputRef
      mempty

    let
      typeaheadElem = div
        [ class_ typeahead
        , tabIndex 0
        ]
        if isActive then
          Array.cons
            ( searchInput
                [ ref searchInputRef
                , placeholder placeholderText
                , onValueInput putOptionText
                , onKeyDown $ mkKeyAction handleKeyDown
                , onFocusOut handleFocusOut
                ]
            )
            (mapWithIndex renderOption currentOptions)
        else
          [ button
              [ class_ typeaheadButton
              , onClick $ const toggleActive
              ]
              [ text $ foldMap show chosenOption ]
          ]

      renderOption n option =
        div
          [ classes $ [ typeaheadOption ]
              <>? (selectedOptionId == n)
              /\ selectedTypeaheadOption
          , style "border-spacing: collapse"
          , onMouseEnter $ const $ putSelectedOptionId n
          , onClick $ const $ chooseOption $ Just option
          ]
          [ text $ show option ]

      currentOptions = Array.take maxOptions
        $ filter (String.startsWith optionText <<< show)
        $ Array.fromFoldable allOptions

      handleFocusOut ev = do
        let
          relatedTargetNode = Node.fromEventTarget =<< relatedTarget ev
          targetNode = Node.fromEventTarget =<< getTarget ev

        fromSameContainer <- liftEffect
          case relatedTargetNode /\ targetNode of
            Just x /\ Just y -> Node.contains x y
            _ -> pure false
        when
          (not fromSameContainer)
          (modifyActive $ const false)

      handleKeyDown keyCode ev
        | keyCode == Enter
            || (keyCode == Tab && Array.length currentOptions == one) =
            withPrevent ev $ chooseOption
              (currentOptions !! selectedOptionId)
        | keyCode == ArrowUp || keyCode == Tab && shiftKey ev =
            withPrevent ev
              $ modifySelectedOptionId dec
        | keyCode == ArrowDown || keyCode == Tab =
            withPrevent ev $ modifySelectedOptionId inc
        | otherwise =
            pure unit

      modifySelectedOptionId f = putSelectedOptionId
        $ clampArrayIndex currentOptions
        $ f selectedOptionId

      toggleActive = putOptionText mempty
        *> putSelectedOptionId zero
        *> modifyActive not

      chooseOption option = putOption option
        *> toggleActive
        *> Hooks.raise outputToken (SelectedOption option)

    useQuery queryToken case _ of
      ActivateTypeahead next -> toggleActive *> pure (Just next)

    Hooks.pure typeaheadElem

searchInputRef :: RefLabel
searchInputRef = RefLabel "searchInput"

type TypeaheadInput a =
  { allOptions :: Set a
  , initialOption :: Maybe a
  , maxOptions :: Int
  , placeholderText :: String
  }

data TypeaheadQuery a = ActivateTypeahead a

data TypeaheadOutput a = SelectedOption (Maybe a)

type TypeaheadSlot a = Slot TypeaheadQuery (TypeaheadOutput a) Unit
