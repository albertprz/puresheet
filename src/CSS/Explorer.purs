module App.CSS.Explorer where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  div &. explorerContainer ? Rule.do
    display := flex
    justifyContent := center
    overflow := auto

  span &. functionName ? Rule.do
    fontSize := termTypeFontSize
    fontWeight := bold

  span &. functionDoc ? Rule.do
    fontSize := termTypeFontSize
    fontFamily := math
    fontWeight := normal

  div &. functionContainer ? Rule.do
    position := fixed
    display := flex
    flexDirection := column
    paddingLeft := pct 10
    paddingRight := pct 10
    backgroundColor := white

  div &. functionContainer |> div &. functionFiltersContainer ? Rule.do
    display := flex
    flexDirection := row
    justifyContent := center
    width := pct 100
    marginTop := px 40
    paddingBottom := px 30

  div &. functionFiltersContainer |> div &. searchInputContainer ? Rule.do
    marginLeft := px 150
    width := px 400
    height := px 40
    justifyContent := center

  div &. functionFiltersContainer |> div &. searchInputContainer
    |> (input &. searchInput)
    ? Rule.do
        width := px 370

  table &. functionsList ? Rule.do
    marginTop := px 320
    maxWidth := pct 70

  tr &. functionRow ? Rule.do
    justifyContent := end
    borderStyle := solid
    borderWidth := px 1.5
    borderColor := lightGrey
    cursor := pointer

  tr &: focus ? Rule.do
    outlineStyle := solid
    outlineWidth := px 1.5
    outlineColor := blue

  tr &. functionRow |> td ? Rule.do
    padding := px 10 ~ px 40

  td &. termTypeLabel ? Rule.do
    fontSize := termTypeFontSize
    color := darkerGrey
    fontStyle := italic
    textAlign := right

  td &. functionDescription ? Rule.do
    fontSize := functionDescriptionFontSize
    textAlign := left
