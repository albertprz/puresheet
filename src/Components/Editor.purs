module App.Components.Editor where

import FatPrelude

import App.Components.Editor.Handler (handleAction, handleQuery)
import App.Components.Editor.Models (EditorAction(..), EditorInput, EditorOutput, EditorQuery, EditorState)
import App.Components.Editor.Renderer (render)
import Halogen (Component, Slot, defaultEval, mkComponent, mkEval)

component
  :: forall m. MonadAff m => Component EditorQuery EditorInput EditorOutput m
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState :: EditorInput -> EditorState
initialState { formulaState } =
  { formulaState
  , suggestions: []
  , selectedSuggestionId: zero
  }

type EditorSlot = Slot EditorQuery EditorOutput Unit

_editor :: Proxy "editor"
_editor = Proxy
