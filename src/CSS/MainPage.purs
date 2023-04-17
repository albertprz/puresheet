module App.CSS.MainPage where

import CSSPrelude
import Tecton.Rule as Rule

import App.CSS.Table as Table

css :: CSS
css = do
  Table.css

  body ? Rule.do
    margin := px 0
    padding := px 0
