module App.CSS.Spreadsheet where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  spreadsheetContainerCss
  formulaSectionCss
  tableCss
  cellCss

spreadsheetContainerCss :: CSS
spreadsheetContainerCss =
  div &. spreadsheetContainer ? Rule.do
    backgroundColor := white
    display := inlineTable

formulaSectionCss :: CSS
formulaSectionCss = do

  div &. formulaSectionContainer ? Rule.do
    position := sticky
    left := px 0
    top := px 0
    width := vw 100
    display := flex
    flexDirection := row
    alignItems := center
    justifyContent := center

  input &. selectedCellInput /\ input &. formulaCellInput ? Rule.do
    height := em 0.7
    width := em 3
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

  tbl ? Rule.do
    borderStyle := solid
    borderWidth := px 0
    borderColor := grey
    color := darkerGrey
    marginRight := vw 50
    display := inlineTable

  universal &: focus ? Rule.do
    outlineWidth := px 0

cellCss :: CSS
cellCss = do

  tbl |* td /\ tbl |* th ? Rule.do
    borderStyle := solid
    borderColor := inherit
    borderWidth := px 0 ~ px 0 ~ px 1 ~ px 1
    padding := em 0.5 ~ em 1
    fontSize := px 16
    fontWeight := normal
    textAlign := center

  tbl |* td &: lastChild /\ tbl |* th &: lastChild ? Rule.do
    borderRightWidth := px 1

  tbl |* td ? Rule.do
    cursor := cell

  tbl |* td |> input ? Rule.do
    backgroundColor := inherit
    borderWidth := px 0
    textAlign := center
    cursor := cell
    fontFamily := "Arial" /\ sansSerif
    fontSize := px 15

  tbl |* td &. inSelection ? Rule.do
    backgroundColor := lighterGreen

  tbl |* td &. aboveSelection /\ td &. belowSelection ? Rule.do
    borderBottomColor := green

  tbl |* td &. atLeftSelection /\ td &. atRightSelection ? Rule.do
    borderLeftColor := green

  tbl &. copySelection |* td &. aboveSelection
    /\ (table &. copySelection |* td &. belowSelection)
    ? Rule.do
        borderBottomStyle := dashed

  tbl &. copySelection |* td &. atLeftSelection
    /\ (table &. copySelection |* td &. atRightSelection)
    ? Rule.do
        borderLeftStyle := dashed

  tbl |* td &. selectedSheetCell ? Rule.do
    outlineStyle := solid
    outlineColor := green
    outlineWidth := px 3
    outlineOffset := px (-3)

  tbl |* th ? Rule.do
    backgroundColor := lighterGrey

  tbl |* th &. selectedHeader ? Rule.do
    backgroundColor := lightGreen
    fontWeight := bold

  tbl |* th &. cornerHeader ? Rule.do
    position := sticky
    top := px 0
    left := px 0
    zIndex := 1
    backgroundColor := lightGrey
    cursor := pointer

  tbl |* th &. columnHeader ? Rule.do
    top := px 0
    cursor := grab

  tbl |* th &. rowHeader ? Rule.do
    position := sticky
    left := px 0
    cursor := grab

tbl :: Selector Extensible
tbl = table &. spreadsheetTable
