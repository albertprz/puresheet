module App.CSS.Table where

import CSSPrelude

import App.CSS.ClassNames (formulaBoxContainer, functionSignature, functionSyntax, suggestionOption, suggestionsDropdown)
import App.CSS.Common (darkGreen, darkPink, formulaFontSize, signatureFontSize, suggestionsFontSize)
import Tecton.Internal (Length, Measure)
import Tecton.Rule as Rule
import Type.Prelude (Proxy)

css :: CSS
css = do

  mainContainerCss
  formulaSectionContainerCss
  formulaCss
  selectedCellInputCss
  formulaCellInputCss
  tableCss
  cellCss

mainContainerCss :: CSS
mainContainerCss = do

  div &. mainContainer ? Rule.do
    backgroundColor := white
    display := inlineTable

formulaSectionContainerCss :: CSS
formulaSectionContainerCss = do

  div &. formulaSectionContainer ? Rule.do
    display := flex
    flexDirection := row
    alignItems := center

formulaCss :: CSS
formulaCss = do

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
    width := rem 16
    backgroundColor := white

  universal &. suggestionOption ? Rule.do
    display := flex
    padding := px 4 ~ px 0
    alignContent := center
    cursor := pointer
    borderWidth := px 1
    borderStyle := solid
    borderColor := grey2
    fontSize := suggestionsFontSize

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

selectedCellInputCss :: CSS
selectedCellInputCss = do

  input &. selectedCellInput ? Rule.do
    left := pct 7.5
    cellInputCommonCss

formulaCellInputCss :: CSS
formulaCellInputCss = do

  input &. formulaCellInput ? Rule.do
    left := pct 85
    cellInputCommonCss

cellInputCommonCss
  :: Declarations (Proxy ("outline-offset" :: Measure Length))
cellInputCommonCss = do

  position := sticky
  height := vh 1.5
  width := vw 3
  margin := px 25
  padding := px 15
  borderColor := green
  borderWidth := px 3
  fontSize := formulaFontSize
  textAlign := center
  outlineStyle := solid
  outlineColor := green
  outlineWidth := px 3
  outlineOffset := px (-3)

tableCss :: CSS
tableCss = do

  table ? Rule.do
    borderStyle := solid
    borderWidth := px 0
    borderColor := grey
    color := darkGrey
    marginRight := px 500
    display := inlineTable

  table &. strippedSheet |> tbody |> tr &: nthChild odd |> td ? Rule.do
    backgroundColor := lighterGrey

  universal &: focus ? Rule.do
    outlineWidth := px 0

cellCss :: CSS
cellCss = do

  td /\ th ? Rule.do
    borderStyle := solid
    borderColor := inherit
    borderWidth := px 0 ~ px 0 ~ px 1 ~ px 1
    padding := em 0.5 ~ em 1
    fontSize := px 16
    fontWeight := normal
    textAlign := center

  td &: lastChild /\ th &: lastChild ? Rule.do
    borderRightWidth := px 1

  td ? Rule.do
    cursor := cell

  td |> input ? Rule.do
    backgroundColor := inherit
    borderWidth := px 0
    textAlign := center
    cursor := cell
    fontFamily := "Arial" /\ sansSerif
    fontSize := px 15

  td &. inSelection ? Rule.do
    backgroundColor := lighterGreen

  td &. aboveSelection /\ td &. belowSelection ? Rule.do
    borderBottomColor := green

  td &. atLeftSelection /\ td &. atRightSelection ? Rule.do
    borderLeftColor := green

  table &. copySelection |* td &. aboveSelection
    /\ (table &. copySelection |* td &. belowSelection)
    ? Rule.do
        borderBottomStyle := dashed

  table &. copySelection |* td &. atLeftSelection
    /\ (table &. copySelection |* td &. atRightSelection)
    ? Rule.do
        borderLeftStyle := dashed

  td &. selectedSheetCell ? Rule.do
    outlineStyle := solid
    outlineColor := green
    outlineWidth := px 3
    outlineOffset := px (-3)

  th ? Rule.do
    backgroundColor := lighterGrey
    cursor := grab

  th &. selectedHeader ? Rule.do
    backgroundColor := lightGreen
    fontWeight := bold

  th &. cornerHeader ? Rule.do
    position := sticky
    top := px 0
    left := px 0
    zIndex := 1
    backgroundColor := lightGrey

  th &. columnHeader ? Rule.do
    top := px 0

  th &. rowHeader ? Rule.do
    position := sticky
    left := px 0
