module Main where

import FatPrelude

import App.Components.MainPage as MainPage
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI MainPage.component unit body
