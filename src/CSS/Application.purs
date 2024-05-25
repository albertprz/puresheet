module App.CSS.Application where

import CSSPrelude

import App.CSS.Editor as Editor
import App.CSS.Explorer as Explorer
import App.CSS.FunctionEditor as FunctionEditor
import App.CSS.HeaderMenu as HeaderMenu
import App.CSS.Spreadsheet as Spreadsheet
import App.CSS.Typeahead as Typeahead
import Tecton.Rule as Rule

css :: CSS
css = do

  componentsCss
  viewsCss
  appCss

componentsCss :: CSS
componentsCss = do
  HeaderMenu.css
  Editor.css
  Typeahead.css
  FunctionEditor.css

viewsCss :: CSS
viewsCss = do
  Spreadsheet.css
  Explorer.css

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

  div &. flexRow ? Rule.do
    display := flex
    flexDirection := row
    alignItems := center
    justifyItems := center
    alignContent := center
    justifyContent := center

  div &. flexColumn ? Rule.do
    display := flex
    flexDirection := column
    alignItems := center
    justifyItems := center
    alignContent := center
    justifyContent := center

  div &. modalContainer ? Rule.do
    zIndex := 1
    position := absolute
    left := px 0
    top := px 0
    height := vh 10000000
    width := vw 100
    backgroundColor := modalBackground

  div &. modalInnerContainer ? Rule.do
    position := fixed
    width := vw 100
    height := vh 100
    display := flex
    justifyContent := center
    alignItems := center

  input ? Rule.do
    fontSize := inputFontSize

  button ? Rule.do
    fontSize := inputFontSize
    cursor := pointer

  button &. addButton ? Rule.do
    display := flex
    alignItems := center
    padding := px 3
    paddingRight := px 10

  button &. addButton |> i ? Rule.do
    marginRight := px 5

  button &. submitButton ? Rule.do
    width := px 150
    height := px 35
