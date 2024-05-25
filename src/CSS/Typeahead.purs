module App.CSS.Typeahead where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  div &. typeahead ? Rule.do
    position := relative

  div &. typeaheadMenu ? Rule.do
    position := absolute
    left := px (-10)
    zIndex := 1

  div &. typeahead /\ button &. typeaheadButton ? Rule.do
    width := px typeaheadWidth

  div &. typeahead |* universal /\ div &. searchInputContainer |* universal ?
    Rule.do
      fontSize := inputFontSize
      justifyContent := center
      boxSizing := borderBox
      padding := px 0 ~ px 10
      height := px 40

  div &. searchInputContainer ? Rule.do
    display := flex
    whiteSpace := nowrap
    borderStyle := solid
    borderColor := darkGrey
    alignItems := center
    backgroundColor := white

  div &. searchInputContainer |> universal ? Rule.do
    padding := px 0
    alignContent := center

  input &. searchInput ? Rule.do
    borderStyle := none
    width := px searchInputWidth
    height := px 30
    padding := px 0 ~ px 10

  div &. typeaheadOption ? Rule.do
    alignContent := center
    textAlign := center
    borderStyle := solid
    borderWidth := px 1.5
    borderColor := darkGrey
    width := px typeaheadWidth
    backgroundColor := white

  div &. selectedTypeaheadOption ? Rule.do
    backgroundColor := lightBlue

  where
  typeaheadWidth = 190
  searchInputWidth = typeaheadWidth - 35
