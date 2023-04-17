module App.Components.Table.Renderer where

import App.CSS.Table
import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellValue, Column, Row, parseCellValue, showCell)
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
    , HP.style "border-spacing: 0"
    , HE.onKeyDown \ev -> KeyPress (KeyboardEvent.code ev) ev
    ]
    [ HH.thead_
        [ HH.tr_
            $ toArray
            $ renderHeaderCorner
            : (renderColumnHeaderCell selectedCell <$> columns)
        ]
    , HH.tbody_ $ toArray $
        do
          row <- rows
          pure $ HH.tr_
            $ toArray
            $ renderRowHeaderCell selectedCell row
            :
              do
                column <- columns
                let
                  cell = { column, row }
                  value = Map.lookup cell tableData
                  selected = cell == selectedCell
                pure $ renderBodyCell selected activeInput cell value
    ]

renderRowHeaderCell :: forall i a. Cell -> Row -> HH.HTML i a
renderRowHeaderCell cell row =
  HH.th
    [ HP.id $ show row
    , HP.classes $ [ rowHeader ] <>
        if cell.row == row then [ selectedHeader ] else []
    ]
    [ HH.text $ show row ]

renderColumnHeaderCell :: forall i. Cell -> Column -> HH.HTML i Action
renderColumnHeaderCell cell column =
  HH.th
    [ HP.id $ show column
    , HP.classes $ [ columnHeader ] <>
        if cell.column == column then [ selectedHeader ] else []
    , HP.draggable true
    , HP.style "cursor: grab"
    , HE.onDragStart $ const $ DragHeader column
    , HE.onDrop $ const $ DropHeader column
    , HE.onDragOver $ DragOverHeader
    ]
    [ HH.text $ show column ]

renderHeaderCorner :: forall i a. HH.HTML i a
renderHeaderCorner =
  HH.th
    [ HP.class_ cornerHeader ]
    [ HH.text mempty ]

renderBodyCell :: forall i. Boolean -> Boolean -> Cell -> Maybe CellValue -> HH.HTML i Action
renderBodyCell selected active cell value =
  HH.td
    [ HP.id $ showCell cell
    , HP.tabIndex 0
    , HP.style "cursor: cell"
    , HP.classes $ [ tableCell ] <> whenMonoid selected [ selectedCell ]
    , HE.onClick $ ClickCell cell
    , HE.onDoubleClick $ DoubleClickCell cell
    ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.autocomplete AutocompleteOff
        , HP.disabled $ not $ selected && active
        , HP.value $ fold $ show <$> value
        , HP.style "cursor: cell"
        , HE.onValueChange $ WriteCell cell <<< parseCellValue
        , HE.onKeyDown \ev -> InputKeyPress (KeyboardEvent.code ev) ev
        ]
    ]

