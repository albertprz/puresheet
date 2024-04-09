module App.Components.MainPage where

import FatPrelude

import App.CSS.MainPage as MainPage
import App.Components.Table as Table
import CSSPrelude (ComponentHTML)
import Halogen (Component, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML (div_, slot_)
import Tecton.Halogen (styleSheet)

component :: forall q m. MonadAff m => Component q Unit Unit m
component =
  mkComponent
    { initialState: const Nothing
    , render
    , eval: mkEval defaultEval
    }

render :: forall i m. MonadAff m => i -> ComponentHTML Unit Slots m
render = const $
  div_
    [ styleSheet MainPage.css
    , slot_ _table unit Table.component unit
    ]

type Slots = (table :: forall q. Slot q Unit Unit)

_table :: Proxy "table"
_table = Proxy
