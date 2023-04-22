module App.CSS.Table where

import CSSPrelude

import Tecton.Rule as Rule

strippedTable :: ClassName
strippedTable = ClassName "stripped-table"

selectedCell :: ClassName
selectedCell = ClassName "selected-cell"

inSelection :: ClassName
inSelection = ClassName "in-selection"

aboveSelection :: ClassName
aboveSelection = ClassName "above-selection"

belowSelection :: ClassName
belowSelection = ClassName "below-selection"

atLeftSelection :: ClassName
atLeftSelection = ClassName "at-left-selection"

atRightSelection :: ClassName
atRightSelection = ClassName "at-right-selection"

tableCell :: ClassName
tableCell = ClassName "table-cell"

columnHeader :: ClassName
columnHeader = ClassName "column-header"

selectedHeader :: ClassName
selectedHeader = ClassName "selected-header"

rowHeader :: ClassName
rowHeader = ClassName "row-header"

cornerHeader :: ClassName
cornerHeader = ClassName "corner-header"

css :: CSS
css = do
  tableCss
  cellCss

tableCss :: CSS
tableCss = do

  table ? Rule.do
    borderStyle := solid
    borderWidth := px 0
    borderColor := grey
    color := darkGrey
    marginRight := px 500
    display := inlineTable

  table &. strippedTable |> tbody |> tr &: nthChild odd |> td ? Rule.do
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

  td |> input ? Rule.do
    backgroundColor := inherit
    borderWidth := px 0
    textAlign := center

  td &. inSelection ? Rule.do
    backgroundColor := lighterGreen

  td &. aboveSelection ? Rule.do
    borderBottomColor := green

  td &. belowSelection ? Rule.do
    borderBottomColor := green

  td &. atLeftSelection ? Rule.do
    borderLeftColor := green

  td &. atRightSelection ? Rule.do
    borderLeftColor := green

  th ? Rule.do
    backgroundColor := lighterGrey
    color := black

  td &. selectedCell ? Rule.do
    outlineStyle := solid
    outlineColor := green
    outlineWidth := px 3
    outlineOffset := px (-3)

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

