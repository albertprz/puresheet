module App.Components.Table.Handler where

import FatPrelude

import App.Components.Table.Cell (nextCell, prevCell, showCell)
import App.Components.Table.Models (Action(..), State)
import Data.Map as Map
import Halogen as H
import Halogen.Aff as HA
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

handleAction
  :: forall slots o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action slots o m Unit

handleAction (WriteCell cell value) =
  H.modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }

handleAction (KeyPress "Tab" ev) = do
  liftEffect $ preventDefault $ KeyboardEvent.toEvent ev
  handleAction $ SelectCell selectFn
  where
  selectFn
    | KeyboardEvent.shiftKey ev = prevCell
    | otherwise = nextCell

handleAction (KeyPress _ _) =
  H.modify_ \st -> st
    { activeInput = true
    }

handleAction (SelectCell cellFn) = do
  cell <- H.gets \st -> cellFn st.selectedCell
  H.modify_ \st -> st
    { selectedCell = cell
    , activeInput = false
    }
  element <- H.liftAff $ HA.selectElement $ QuerySelector $ "td#" <> showCell cell
  H.liftEffect $ fromMaybe (pure unit) (focus <$> element)
