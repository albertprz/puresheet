module App.Components.Typeahead where

import FatPrelude hiding (div)

import App.CSS.ClassNames (selectedTypeaheadOption, typeahead, typeaheadButton, typeaheadMenu, typeaheadOption)
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
  => Component (TypeaheadQuery a) (TypeaheadInput a) (TypeaheadOutput a) Aff
component = Hooks.component
  \tokens
   { allOptions, initialOption, maxOptions, placeholder: placeholderText } ->
    Hooks.do

      chosenOption /\ putOption <- usePutState initialOption
      optionText /\ putOptionText <- usePutState mempty
      selectedOptionId /\ putSelectedOptionId <- usePutState zero
      active /\ modifyActive <- useModifyState_ false

      Hooks.captures { active } useTickEffect do
        when active $ focusHooksRef searchInputRef
        mempty

      let
        typeaheadElem = div
          [ class_ typeahead
          , tabIndex zero
          ]
          if active then
            [ searchInput
                [ ref searchInputRef
                , placeholder placeholderText
                , onValueInput putOptionText
                , onKeyDown $ mkKeyAction handleKeyDown
                , onFocusOut handleFocusOut
                ]
            , div [ class_ typeaheadMenu ]
                (mapWithIndex renderOption currentOptions)
            ]
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
            , onClick $ const $ chooseOption option
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
                $ unsafeFromJust (currentOptions !! selectedOptionId)
          | keyCode == ArrowUp || keyCode == Tab && shiftKey ev =
              withPrevent ev
                $ modifySelectedOptionId dec
          | keyCode == ArrowDown || keyCode == Tab =
              withPrevent ev $ modifySelectedOptionId inc
          | otherwise =
              pure unit

        modifySelectedOptionId f =
          putSelectedOptionId
            $ clampArrayIndex currentOptions
            $ f selectedOptionId

        toggleActive =
          putOptionText mempty
            *> putSelectedOptionId zero
            *> modifyActive not

        chooseOption option =
          putOption (Just option)
            *> toggleActive
            *> Hooks.raise tokens.outputToken (SelectedOption option)

      useQuery tokens.queryToken case _ of
        ActivateTypeahead next -> do
          toggleActive
          pure (Just next)
        SelectOption option next -> do
          putOption (Just option)
          pure (Just next)

      Hooks.pure typeaheadElem

searchInputRef :: RefLabel
searchInputRef = RefLabel "searchInput"

type TypeaheadInput a =
  { allOptions :: Set a
  , initialOption :: Maybe a
  , maxOptions :: Int
  , placeholder :: String
  }

data TypeaheadQuery a b = ActivateTypeahead b | SelectOption a b

data TypeaheadOutput a = SelectedOption a

type TypeaheadSlot a = Slot (TypeaheadQuery a) (TypeaheadOutput a) Unit
