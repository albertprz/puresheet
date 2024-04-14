module App.CSS.Application where

import CSSPrelude

import App.CSS.ClassNames (invisibleContainer)
import App.CSS.Editor as Editor
import App.CSS.HeaderMenu as HeaderMenu
import App.CSS.Spreadsheet as Spreadsheet
import Tecton.Rule as Rule

css :: CSS
css = do

  appCss
  HeaderMenu.css
  Editor.css
  Spreadsheet.css

appCss :: CSS
appCss = do

  body ? Rule.do
    margin := px 0
    padding := px 0
    overflow := hidden

  universal &. invisibleContainer ? Rule.do
    display := none
