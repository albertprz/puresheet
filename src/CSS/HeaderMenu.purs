module App.CSS.HeaderMenu where

import CSSPrelude

import Tecton.Rule as Rule

css :: CSS
css = do

  universal &. headerMenu ? Rule.do
    display := flex

  button &. navButton ? Rule.do
    zIndex := 1
    position := fixed
    marginTop := pct 2
    marginLeft := pct 3
    left := px 0
    fontSize := px 40
    cursor := pointer
