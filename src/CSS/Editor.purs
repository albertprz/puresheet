module App.CSS.Editor where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  div &. formulaBoxContainer ? Rule.do
    display := flex
    flexDirection := column

  div &. formulaBox ? Rule.do
    width := vw 50
    height := px 100
    marginLeft := vw 6.5
    marginRight := vw 6.5
    marginTop := px 50
    padding := px 20 ~ px 25
    borderStyle := solid
    borderColor := darkGrey
    borderWidth := px 3
    fontFamily := monospace
    fontSize := formulaFontSize
    whiteSpace := breakSpaces
    overflow := auto

  div &. formulaBox &:: PseudoElement "-webkit-scrollbar" ? Rule.do
    display := none

  div &. suggestionsDropdown ? Rule.do
    position := fixed
    width := rem 10
    backgroundColor := white

  div &. suggestionOption ? Rule.do
    display := flex
    padding := px 4 ~ px 0
    alignContent := center
    cursor := pointer
    borderWidth := px 1
    borderStyle := solid
    borderColor := darkGrey
    fontSize := formulaFontSize

  div &. selectedSuggestionOption ? Rule.do
    backgroundColor := lightBlue

  div &. functionSignature ? Rule.do
    height := px 20
    marginTop := px 15
    marginBottom := px 25
    padding := px 0 ~ px 50
    fontSize := signatureFontSize
    textAlign := center

  div &. unknownFormula ? Rule.do
    backgroundColor := lighterYellow
    borderColor := yellow

  div &. validFormula ? Rule.do
    backgroundColor := lighterGreen
    borderColor := green

  div &. invalidFormula ? Rule.do
    backgroundColor := lighterRed
    borderColor := red

  span &. keywordSyntax ? Rule.do
    color := darkPink

  span &. functionSyntax ? Rule.do
    color := blue

  span &. symbolSyntax ? Rule.do
    color := red

  span &. operatorSyntax ? Rule.do
    color := red

  span &. cellSyntax ? Rule.do
    color := purple

  span &. numberSyntax ? Rule.do
    color := purple

  span &. stringSyntax ? Rule.do
    color := darkGreen
