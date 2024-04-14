module App.CSS.HeaderMenu where

import CSSPrelude

import App.CSS.ClassNames (headerMenu, navButton)
import Tecton.Rule as Rule

css :: CSS
css = do

  universal &. headerMenu ? Rule.do
    display := flex

  button &. navButton ? Rule.do
    marginTop := pct 2
    marginLeft := pct 3
    position := absolute
    left := px 0
    fontSize := px 40
    cursor := pointer
