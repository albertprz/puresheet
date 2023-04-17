module App.CSS.Table where

import CSSPrelude

import App.CSS.Common (green, lightGreen)
import Tecton.Rule as Rule

strippedTable :: ClassName
strippedTable = ClassName "stripped-table"

selectedCell :: ClassName
selectedCell = ClassName "selected-cell"

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

  table &. strippedTable |> tbody |> tr &: nthChild odd |> td ? Rule.do
    backgroundColor := lighterGrey

  universal &: focus ? Rule.do
    outlineWidth := px 0

cellCss :: CSS
cellCss = do

  th /\ td ? Rule.do
    borderStyle := solid
    borderWidth := px 0 ~ px 0 ~ px 1 ~ px 1
    borderColor := inherit
    textAlign := center
    padding := em 0.5 ~ em 1
    overflow := visible

  th ? Rule.do
    backgroundColor := lightGrey
    color := black

  td |> input ? Rule.do
    textAlign := center

  td &. selectedCell ? Rule.do
    outlineColor := green
    outlineStyle := solid
    outlineWidth := px 3
    outlineOffset := px (-3)

  tbody |> tr &: lastChild |> td ? Rule.do
    borderBottomWidth := px 0

  td &: firstChild ? Rule.do
    borderLeftWidth := px 0

  th &. selectedHeader ? Rule.do
    backgroundColor := lightGreen

  th &. columnHeader ? Rule.do
    position := sticky
    top := px 0

  th &. rowHeader ? Rule.do
    position := sticky
    left := px 0

  th &. cornerHeader ? Rule.do
    position := sticky
    left := px 0
    top := px 0
