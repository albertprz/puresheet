module App.CSS.MainPage where

import CSSPrelude

import App.CSS.Table as Table
import Tecton.Rule as Rule

css :: CSS
css = do

  Table.css

  body ? Rule.do
    margin := px 0
    padding := px 0
    overflow := hidden
