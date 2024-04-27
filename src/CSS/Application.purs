module App.CSS.Application where

import CSSPrelude

import App.CSS.Editor as Editor
import App.CSS.Explorer as Explorer
import App.CSS.HeaderMenu as HeaderMenu
import App.CSS.Spreadsheet as Spreadsheet
import App.CSS.Typeahead as Typeahead
import Tecton.Rule as Rule

css :: CSS
css = do

  componentsCss
  viewsCss
  appCss

viewsCss :: CSS
viewsCss = do
  Spreadsheet.css
  Explorer.css

componentsCss :: CSS
componentsCss = do
  HeaderMenu.css
  Editor.css
  Typeahead.css

appCss :: CSS
appCss = do

  body ? Rule.do
    margin := px 0
    padding := px 0

  body &:: PseudoElement "-webkit-scrollbar" ? Rule.do
    display := none

  universal &. invisibleContainer ? Rule.do
    display := none

  universal &. hiddenContainer ? Rule.do
    visibility := hidden
