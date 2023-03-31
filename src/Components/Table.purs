module App.Components.Table where

import Color
import Prelude
import Tecton

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tecton.Halogen as TH
import Tecton.Rule as Rule


type State
  = { count :: Int }

data Action
  = Increment

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render: renderTable
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }


tableStyle = Rule.do
  margin := px 0
  padding := px 5
  backgroundColor := black
  color := white


cellStyle = Rule.do
  borderStyle := solid
  borderWidth := px 10



renderTable :: forall cs m. State -> H.ComponentHTML Action cs m
renderTable _ = HH.table
                    [ TH.style tableStyle ]
                    [ HH.tr_
                      [ HH.th
                        [ TH.style cellStyle  ]
                        [ HH.text "Run Ads On:"]
                      , HH.td
                        [ TH.style cellStyle  ]
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
