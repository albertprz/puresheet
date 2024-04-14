module App.Components.Explorer.Renderer where

import FatPrelude hiding (div)

import App.CSS.ClassNames (explorerContainer, invisibleContainer)
import App.Components.Explorer.Models (ExplorerState)
import App.Routes (Route(..))
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties (class_)

render :: forall w i. ExplorerState -> HTML w i
render { route } = div
  [ class_
      if route == ExplorerView then explorerContainer
      else invisibleContainer
  ]
  []
