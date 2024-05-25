module App.CSS.FunctionEditor where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  div &. functionEditorModal ? Rule.do
    display := flex
    flexDirection := column
    width := vw 90
    height := vh 90
    backgroundColor := white
    justifyContent := center
    alignItems := center

  div &. functionEditorModal |* div &. formulaBox ? Rule.do
    height := vh 35
    width := vw 70

  div &. functionEditorModal |* div &. formulaBox &: empty &:: before ? Rule.do
    color := placeholderColor
    content := "Function body"

  div &. functionEditorModal |> div &. flexRow &: nthChild (AnPlusB 1 2)
    |> universal
    ? Rule.do
        marginTop := px 100

  div &. functionEditorModal |> div &. flexRow &: lastChild
    ? Rule.do
        marginTop := px (-50)

  div &. operatorEditorModal |> div &. flexRow &: lastChild
    ? Rule.do
        position := absolute
        bottom := vh 10

  div &. functionEditorModal |> div &. flexRow |> universal &: firstChild
    ? Rule.do
        marginRight := px 100

  div &. functionEditorModal |> div &. flexRow |> universal &: lastChild
    ? Rule.do
        marginLeft := px 100

  div &. operatorEditorModal |> div &. flexRow |> universal &: firstChild
    ? Rule.do
        marginRight := px 150

  div &. operatorEditorModal |> div &. flexRow |> universal &: lastChild
    ? Rule.do
        marginLeft := px 125

  div &. operatorEditorModal |> div &. flexRow &: lastChild |> universal
    &: firstChild
    ? Rule.do
        marginRight := px 100

  div &. operatorEditorModal |> div &. flexRow &: lastChild |> universal
    &: lastChild
    ? Rule.do
        marginLeft := px 100

  input &. functionNameInput ? Rule.do
    width := px 150
    padding := px 0 ~ px 15
    height := px 40

  input &. functionSignatureInput ? Rule.do
    width := px 500
    padding := px 0 ~ px 15
    height := px 40

  textarea &. functionDocString ? Rule.do
    width := vw 70
    marginTop := px 30
    paddingBottom := px 0
