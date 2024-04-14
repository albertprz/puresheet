module App.Components.Explorer where

import FatPrelude hiding (div)

import App.AppM (AppM)
import App.AppStore (Store)
import App.CSS.ClassNames (explorerContainer, invisibleContainer)
import App.Components.Explorer.Models (Action(..), ExplorerInput, ExplorerState)
import App.Routes (Route(..))
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties (class_)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectAll)
import Record (merge)

component :: forall q. Component q ExplorerInput Unit AppM
component =
  connect selectAll $ mkComponent
    { initialState
    , render
    , eval: mkEval defaultEval
        { receive = Just <<< Receive }
    }

initialState :: { context :: Store, input :: ExplorerInput } -> ExplorerState
initialState { context, input } =
  merge context { route: input.route }

render :: forall w i. ExplorerState -> HTML w i
render { route } = div
  [ class_
      if route == ExplorerView then explorerContainer
      else invisibleContainer
  ]
  []

type ExplorerSlot = forall q. Slot q Unit Unit

_explorer :: Proxy "explorer"
_explorer = Proxy
