module App.CSS.Editor where

import CSSPrelude

import App.CSS.ClassNames (formulaBoxContainer, functionSignature, functionSyntax, selectedSuggestionOption, suggestionOption, suggestionsDropdown)
import App.CSS.Common (darkGreen, darkPink, formulaFontSize, lightBlue, signatureFontSize)
import Tecton.Rule as Rule

css :: CSS
css = do

  div &. formulaBoxContainer ? Rule.do
    display := flex
    flexDirection := column
    position := sticky
    left := pct 23

  universal &. formulaBox ? Rule.do
    width := vw 50
    height := px 100
    margin := px 20
    marginTop := px 40
    marginBottom := px 10
    padding := px 20
    borderStyle := solid
    borderColor := grey2
    borderWidth := px 3
    fontSize := formulaFontSize
    fontWeight := bold
    whiteSpace := breakSpaces
    overflow := auto

  universal &. suggestionsDropdown ? Rule.do
    position := fixed
    width := rem 10
    backgroundColor := white

  universal &. suggestionOption ? Rule.do
    display := flex
    padding := px 4 ~ px 0
    alignContent := center
    cursor := pointer
    borderWidth := px 1
    borderStyle := solid
    borderColor := grey2
    fontSize := formulaFontSize

  universal &. selectedSuggestionOption ? Rule.do
    backgroundColor := lightBlue

  universal &. functionSignature ? Rule.do
    height := px 20
    margin := px 20
    marginTop := px 10
    padding := px 0 ~ px 50
    fontSize := signatureFontSize
    fontWeight := bold
    textAlign := center

  universal &. unknownFormula ? Rule.do
    backgroundColor := lighterYellow
    borderColor := yellow

  universal &. validFormula ? Rule.do
    backgroundColor := lighterGreen
    borderColor := green

  universal &. invalidFormula ? Rule.do
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
