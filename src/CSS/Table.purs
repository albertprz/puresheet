module App.CSS.Table where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do
  mainContainerCss
  formulaCss
  tableCss
  cellCss

mainContainerCss :: CSS
mainContainerCss = do

  div &. mainContainer ? Rule.do
    backgroundColor := white
    display := inlineTable

formulaCss :: CSS
formulaCss = do

  textarea &. formulaBox ? Rule.do
    position := sticky
    left := pct 25
    width := vw 50
    height := vh 12
    margin := px 25
    padding := px 20
    borderColor := grey2
    borderWidth := px 3
    fontSize := px 18

  textarea &. validFormula ? Rule.do
    animationName := blinkerGreen
    animationDelay := sec 0
    animationDuration := sec 0.75
    animationIterationCount := 4

  textarea &. invalidFormula ? Rule.do
    animationName := blinkerRed
    animationDelay := sec 0
    animationDuration := sec 0.75
    animationIterationCount := 4

  keyframes blinkerGreen ? do
    pct 70
      ? borderColor
      := green

  keyframes blinkerRed ? do
    pct 70
      ? borderColor
      := red

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

  td &. inSelection ? Rule.do
    backgroundColor := lighterGreen

  td &. aboveSelection /\ td &. belowSelection ? Rule.do
    borderBottomColor := green

  td &. atLeftSelection /\ td &. atRightSelection ? Rule.do
    borderLeftColor := green

  table &. copySelection |* td &. aboveSelection /\ table &. copySelection |* td
    &. belowSelection
    ? Rule.do
        borderBottomStyle := dashed

  table &. copySelection |* td &. atLeftSelection /\ table &. copySelection
    |* td
    &. atRightSelection
    ? Rule.do
        borderLeftStyle := dashed

  td &. selectedSheetCell ? Rule.do
    outlineStyle := solid
    outlineColor := green
    outlineWidth := px 3
    outlineOffset := px (-3)

  th ? Rule.do
    backgroundColor := lighterGrey
    color := black
    cursor := grab

  th &. selectedHeader ? Rule.do
    backgroundColor := lightGreen

  th &. cornerHeader ? Rule.do
    position := sticky
    top := px 0
    left := px 0
    zIndex := 10

  th &. columnHeader ? Rule.do
    position := sticky
    top := px 0

  th &. rowHeader ? Rule.do
    position := sticky
    left := px 0

blinkerGreen :: KeyframesName
blinkerGreen = KeyframesName "blinker-green"

blinkerRed :: KeyframesName
blinkerRed = KeyframesName "blinker-red"
