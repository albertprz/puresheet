module App.Components.Table.Renderer where

import FatPrelude
import Prim hiding (Row)

import App.CSS.Table (strippedTable)
import App.Components.Table.Cell (Cell, CellValue, Column, parseCellValue, showCell)
import App.Components.Table.Models (Action(..), State)
import DOM.HTML.Indexed.AutocompleteType (AutocompleteType(..))
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KeyboardEvent

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { selectedCell, activeInput, tableData, columns, rows } =
  HH.table
    [ HP.class_ strippedTable
    , HE.onKeyDown \ev -> KeyPress (KeyboardEvent.code ev) ev
    ]
    [ HH.thead_
        [ HH.tr_
            $ toArray
            $ renderHeaderCell
            <$> columns
        ]
    , HH.tbody_ $
        do
          row <- toArray rows
          pure $ HH.tr_ $
            do
              column <- toArray columns
              let
                cell = { column, row }
                value = Map.lookup cell tableData
                active = cell == selectedCell && activeInput
              pure $ renderBodyCell active cell value
    ]

renderHeaderCell :: forall i o. Column -> HH.HTML i o
renderHeaderCell column =
  HH.th
    [ HP.id $ show column ]
    [ HH.text $ show column ]

renderBodyCell :: forall i. Boolean -> Cell -> Maybe CellValue -> HH.HTML i Action
renderBodyCell active cell value =
  HH.td
    [ HP.id $ showCell cell
    , HP.tabIndex 0
    , HE.onClick $ ClickCell cell
    , HE.onDoubleClick $ DoubleClickCell cell
    ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.autocomplete AutocompleteOff
        , HP.disabled $ not active
        , HP.value $ fromMaybe "" $ show <$> value
        , HE.onValueChange $ WriteCell cell <<< parseCellValue
        , HE.onKeyDown \ev -> InputKeyPress (KeyboardEvent.code ev) ev
        ]
    ]

