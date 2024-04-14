module Main where

import FatPrelude

import App.AppStore (getInitialStore, reduce)
import App.Components.Application as MainPage
import App.Routes (routeCodec)
import Halogen (hoist)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Router.Trans.Hash (mkRouter, runRouterT)
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)
import Safe.Coerce (coerce)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  initialStore <- getInitialStore
  router <- liftEffect $ mkRouter routeCodec
  root <- runStoreT initialStore reduce
    $ hoist (runRouterT router)
    $ coerce MainPage.component
  runUI root unit body
