module App.Components.Table.Renderer where

import FatPrelude
import Prim hiding (Row)

import App.CSS.Table (aboveSelection, atLeftSelection, atRightSelection, belowSelection, columnHeader, cornerHeader, inSelection, rowHeader, selectedCell, selectedHeader, tableCell)
import App.Components.Table.Cell (Cell, CellValue, Column, MultiSelection, Row, isCellAboveSelection, isCellAtLeftSelection, isCellAtRightSelection, isCellBelowSelection, isCellInSelection, isColumnSelected, isRowSelected, parseCellValue, showCell)
import App.Components.Table.HandlerHelpers (parseKey)
import App.Components.Table.Models (Action(..), EventTransition(..), Header(..), State)
import CSSPrelude (ClassName)
import DOM.HTML.Indexed.AutocompleteType (AutocompleteType(..))
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KeyboardEvent

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { selectedCell, activeInput, tableData, columns, rows, multiSelection } =
  HH.table
    [ HP.style "border-spacing: 0"
    , HE.onKeyDown \ev -> KeyPress (parseKey $ KeyboardEvent.code ev) ev
    , HE.onWheel WheelScroll
    ]
    [ HH.thead_
        [ HH.tr_
            $ toArray
            $ renderHeaderCorner
            : (renderColumnHeader selectedCell multiSelection <$> columns)
        ]
    , HH.tbody_ $ toArray $
        do
          row <- rows
          pure $ HH.tr_
            $ toArray
            $ renderRowHeader selectedCell multiSelection row
            :
              do
                column <- columns
                let
                  cell = { column, row }
                  cellValue = Map.lookup cell tableData
                pure $ renderBodyCell selectedCell multiSelection activeInput cell cellValue
    ]

renderRowHeader :: forall i. Cell -> MultiSelection -> Row -> HH.HTML i Action
renderRowHeader selected selection row =
  HH.th
    [ HP.id $ show row
    , HP.classes $ [ rowHeader ]
        <>! isRowSelected selected selection row
        /\ selectedHeader
    , HE.onClick $ const $ ClickHeader $ RowHeader row
    ]
    [ HH.text $ show row ]

renderColumnHeader :: forall i. Cell -> MultiSelection -> Column -> HH.HTML i Action
renderColumnHeader selected selection column =
  HH.th
    [ HP.id $ show column
    , HP.classes $ [ columnHeader ]
        <>! isColumnSelected selected selection column
        /\ selectedHeader
    , HP.draggable true
    , HP.style "cursor: grab"
    , HE.onDragStart $ DragHeader Start column
    , HE.onDrop $ DragHeader End column
    , HE.onDragOver $ DragHeader Over column
    , HE.onClick $ const $ ClickHeader $ ColumnHeader column
    ]
    [ HH.text $ show column ]

renderHeaderCorner :: forall i. HH.HTML i Action
renderHeaderCorner =
  HH.th
    [ HP.class_ cornerHeader
    , HE.onClick $ const $ ClickHeader CornerHeader
    ]
    [ HH.text mempty ]

renderBodyCell :: forall i. Cell -> MultiSelection -> Boolean -> Cell -> Maybe CellValue -> HH.HTML i Action
renderBodyCell selected selection active cell value =
  HH.td
    [ HP.id $ showCell cell
    , HP.tabIndex 0
    , HP.style "cursor: cell"
    , HP.classes $ bodyCellSelectionClasses selected selection cell
    , HE.onClick $ ClickCell cell
    , HE.onDoubleClick $ DoubleClickCell cell
    , HE.onMouseDown $ DragCell Start cell
    , HE.onMouseUp $ DragCell End cell
    , HE.onMouseOver $ DragCell Over cell
    ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.autocomplete AutocompleteOff
        , HP.disabled $ not $ cell == selected && active
        , HP.value $ foldMap show value
        , HP.style "cursor: cell"
        , HE.onValueChange $ WriteCell cell <<< parseCellValue
        , HE.onKeyDown \ev -> InputKeyPress (parseKey $ KeyboardEvent.code ev) ev
        ]
    ]

bodyCellSelectionClasses :: Cell -> MultiSelection -> Cell -> Array ClassName
bodyCellSelectionClasses selected selection cell =
  [ tableCell ]
    <>! (selected == cell)
    /\ selectedCell
    <>! isCellInSelection selection cell
    /\ inSelection
    <>! isCellAboveSelection selection cell
    /\ aboveSelection
    <>! isCellBelowSelection selection cell
    /\ belowSelection
    <>! isCellAtLeftSelection selection cell
    /\ atLeftSelection
    <>! isCellAtRightSelection selection cell
    /\ atRightSelection
