module App.Components.Explorer.Handler where

import FatPrelude

import App.AppM (AppM)
import App.Components.Explorer.Models (ExplorerAction(..), ExplorerState)
import Halogen (HalogenM)

handleAction
  :: ExplorerAction
  -> HalogenM ExplorerState ExplorerAction () Unit AppM Unit

handleAction (Receive { input, context }) =
  modify_ _ { route = input.route, store = context }

handleAction (ClickFunctionRow n) =
  modify_ _ { selectedRowNumber = n }
