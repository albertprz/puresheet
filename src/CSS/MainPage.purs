module App.CSS.MainPage where

import App.CSS.Table as Table
import Tecton (CSS)

css :: CSS
css = do
  Table.css
