module App.Components.Table where

import Prelude

import Halogen as H
import Halogen.HTML as HH


type State
  = { count :: Int }

data Action
  = Increment

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }



render :: forall cs m. State -> H.ComponentHTML Action cs m
render _ = HH.table_
                    [ HH.tr_
                      [ HH.th_
                        [ HH.text "Run Ads On :"]
                      , HH.td_
                        [ HH.text "Stack Overflow"]
                      ]
                    , HH.tr_
                      [ HH.th_
                        [ HH.text "Social Account:"]
                      , HH.td_
                        [ HH.text "Dave Loves Gang of Four"]
                      ]
                    , HH.tr_
                      [ HH.th_
                        [ HH.text "Ads Account:"]
                      , HH.td_
                        [ HH.text "123991234"]
                      ]
                      ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
    Increment -> H.modify_ \st -> st { count = st.count + 1 }
