module App.CSS.Table where

import CSSPrelude

import Tecton.Rule as Rule

strippedTable :: ClassName
strippedTable = ClassName "stripped-table"

css :: CSS
css = do
  tableCss
  cellCss

tableCss :: CSS
tableCss = do

  table ? Rule.do
    borderStyle := solid
    borderWidth := px 1
    borderColor := grey
    color := darkGrey

  table |> thead ? Rule.do
    backgroundColor := lightGrey
    color := black

  table &. strippedTable |> tbody |> tr &: nthChild odd |> td ? Rule.do
    backgroundColor := lighterGrey

cellCss :: CSS
cellCss = do

  th /\ td ? Rule.do
    borderStyle := solid
    borderWidth := px 0 ~ px 0 ~ px 1 ~ px 1
    borderColor := inherit
    textAlign := center
    margin := px 0
    padding := em 0.5 ~ em 1
    overflow := visible

  td |> input ? Rule.do
    textAlign := center

  tbody |> tr &: lastChild |> td ? Rule.do
    borderBottomWidth := px 0

  tr |> th &: firstChild /\ td &: firstChild ? Rule.do
    borderLeftWidth := px 0
