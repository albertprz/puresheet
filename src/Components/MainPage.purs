module App.Components.MainPage where

import App.CSS.MainPage as MainPage
import App.Components.Table as Table
import Control.Category (identity)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Halogen as H
import Halogen.HTML as HH
import Tecton.Halogen (styleSheet)
import Type.Prelude (Proxy(..))

-- import Data.Symbol (SProxy(..))


_table = Proxy :: Proxy "table"


type State
  = {}

data Action
  = None

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render _ = HH.div_ [
  styleSheet MainPage.css,
  HH.slot_ _table unit Table.component unit
  ]


handleAction :: forall a cs o m. Action → H.HalogenM a Action cs o m Unit
handleAction = case _ of
    None -> H.modify_ identity
