module Main where

import FatPrelude

import App.Components.AppStore (initialStore, reduce)
import App.Components.MainPage as MainPage
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  root <- runStoreT initialStore reduce MainPage.component
  runUI root unit body
