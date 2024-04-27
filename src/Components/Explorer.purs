module App.Components.Explorer where

import FatPrelude hiding (div)

import App.AppM (AppM)
import App.AppStore (Store)
import App.Components.Explorer.Handler (handleAction)
import App.Components.Explorer.Models (ExplorerAction(..), ExplorerInput, ExplorerState)
import App.Components.Explorer.Renderer (render)
import App.SyntaxTree.Common (preludeModule)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Data.Slot (Slot)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectAll)

component :: forall q. Component q ExplorerInput Unit AppM
component =
  connect selectAll $ mkComponent
    { initialState
    , render
    , eval: mkEval defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState :: { context :: Store, input :: ExplorerInput } -> ExplorerState
initialState { context, input } =
  { route: input.route
  , store: context
  , module': Just preludeModule
  , fnFilter: Nothing
  , selectedRow: zero
  }

type ExplorerSlot = forall q. Slot q Unit Unit

_explorer :: Proxy "explorer"
_explorer = Proxy
