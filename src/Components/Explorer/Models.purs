module App.Components.Explorer.Models where

import App.AppStore (Store, StoreRow)
import App.Routes (Route)

type ExplorerState = { route :: Route | StoreRow }

type ExplorerInput = { route :: Route }

data Action =
  Receive { context :: Store, input :: ExplorerInput }
