module App.Components.Table.Handler where

import FatPrelude

import App.Components.Table.HandlerHelpers (actOnCell, prevent, withPrevent)
import App.Components.Table.Cell (nextCell, nextColumnCell, nextRowCell, prevCell, prevColumnCell, prevRowCell)
import App.Components.Table.Models (Action(..), State)
import Data.Map as Map
import Halogen as H
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action slots o m Unit

handleAction Initialize = do
  cell <- H.gets \st -> st.selectedCell
  handleAction (SelectCell (\_ _ _ -> Just cell))

handleAction (WriteCell cell value) =
  H.modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }

handleAction (ClickCell cell ev) = withPrevent ev
  $ handleAction
  $ SelectCell (\_ _ _ -> Just cell)

handleAction (KeyPress "Tab" ev) = do
  withPrevent ev $ handleAction $ SelectCell selectFn
  where
  selectFn
    | KeyboardEvent.shiftKey ev = prevCell
    | otherwise = nextCell

handleAction (KeyPress "ArrowLeft" ev) = do
  withPrevent ev $ handleAction $ SelectCell prevColumnCell

handleAction (KeyPress "ArrowRight" ev) = do
  withPrevent ev $ handleAction $ SelectCell nextColumnCell

handleAction (KeyPress "ArrowUp" ev) = do
  withPrevent ev $ handleAction $ SelectCell prevRowCell

handleAction (KeyPress "ArrowDown" ev) = do
  withPrevent ev $ handleAction $ SelectCell nextRowCell

handleAction (KeyPress "Space" ev) = do
  prevent ev

handleAction (KeyPress "Enter" _) =
  H.modify_ \st -> st
    { activeInput = true
    }

handleAction (KeyPress _ _) =
  pure unit

handleAction (SelectCell cellFn) = do
  { cell, columns, rows } <- H.gets \st ->
    { cell: st.selectedCell, columns: st.columns, rows: st.rows }
  let newCell = fromMaybe cell $ cellFn columns rows cell
  H.modify_ \st -> st
    { selectedCell = newCell
    , activeInput = false
    }
  actOnCell newCell focus

