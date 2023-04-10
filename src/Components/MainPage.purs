module App.Components.MainPage where

import FatPrelude

import App.CSS.MainPage as MainPage
import App.Components.Table as Table
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Tecton.Halogen (styleSheet)

_table = Proxy :: Proxy "table"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval
    }

render _ = HH.div_
  [ styleSheet MainPage.css
  , HH.slot_ _table unit Table.component unit
  ]
