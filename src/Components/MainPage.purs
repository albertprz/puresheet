module App.Components.MainPage where

import FatPrelude

import App.CSS.MainPage as MainPage
import App.Components.Table as Table
import Halogen as H
import Halogen.HTML as HH
import Tecton.Halogen (styleSheet)

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall s i q o m x. MonadAff m => i -> HH.HTML (Slots s q o m x) s
render = const $
  HH.div_
    [ styleSheet MainPage.css
    , HH.slot_ _table unit Table.component unit
    ]

type Slots s q o m x = H.ComponentSlot (table :: H.Slot q o Unit | x) m s

_table = Proxy :: Proxy "table"
