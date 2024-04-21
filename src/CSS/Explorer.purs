module App.CSS.Explorer where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  div &. explorerContainer ? Rule.do
    display := flex
    justifyContent := center
    overflow := hidden

  span &. functionName ? Rule.do
    fontSize := termTypeFontSize
    fontWeight := bold

  span &. functionDoc ? Rule.do
    fontSize := termTypeFontSize
    fontFamily := math
    fontWeight := normal

  table &. functionsList ? Rule.do
    marginTop := px 250
    maxWidth := pct 80

  tr &. functionRow ? Rule.do
    justifyContent := end
    borderStyle := solid
    borderWidth := px 1
    borderColor := lightGrey
    cursor := pointer

  tr &. functionRow |> td ? Rule.do
    padding := px 10 ~ px 40

  td &. termTypeLabel ? Rule.do
    fontSize := termTypeFontSize
    color := darkGrey
    fontStyle := italic
    textAlign := right

  td &. functionDescription ? Rule.do
    fontSize := functionDescriptionFontSize
    textAlign := left

  div &. functionContainer ? Rule.do
    position := fixed
    display := flex
    justifyContent := center
    width := pct 80
    backgroundColor := white
