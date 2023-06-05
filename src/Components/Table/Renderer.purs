module App.Components.Table.Renderer where

import FatPrelude
import Prim hiding (Row)

import App.CSS.ClassNames
  ( aboveSelection
  , atLeftSelection
  , atRightSelection
  , belowSelection
  , columnHeader
  , copySelection
  , cornerHeader
  , inSelection
  , rowHeader
  , selectedHeader
  , selectedSheetCell
  , sheetCell
  )
import App.Components.Table.Cell
  ( Cell
  , CellValue
  , Column
  , Header(..)
  , MultiSelection
  , Row
  , SelectionState(..)
  , isCellAboveSelection
  , isCellAtLeftSelection
  , isCellAtRightSelection
  , isCellBelowSelection
  , isCellInSelection
  , isColumnSelected
  , isRowSelected
  , parseCellValue
  , showCell
  )
import App.Components.Table.Models (Action(..), EventTransition(..), State)
import App.Utils.Dom (parseKeyCode)
import Data.Map as Map
import Halogen.HTML
  ( ClassName
  , ComponentHTML
  , HTML
  , input
  , table
  , tbody_
  , td
  , text
  , th
  , thead_
  , tr_
  )
import Halogen.HTML.Events
  ( onClick
  , onDoubleClick
  , onDragOver
  , onDragStart
  , onDrop
  , onKeyDown
  , onKeyUp
  , onMouseDown
  , onMouseOver
  , onMouseUp
  , onValueChange
  , onWheel
  )
import Halogen.HTML.Properties
  ( AutocompleteType(..)
  , InputType(..)
  , autocomplete
  , class_
  , classes
  , disabled
  , draggable
  , id
  , style
  , tabIndex
  , type_
  , value
  )
import Web.UIEvent.KeyboardEvent as KeyboardEvent

render :: forall cs m. State -> ComponentHTML Action cs m
render
  { selectedCell
  , activeInput
  , tableData
  , columns
  , rows
  , multiSelection
  , selectionState
  } =
  table
    [ classes $ whenMonoid (selectionState == CopySelection) [ copySelection ]
    , style "border-spacing: 0"
    , onKeyDown \ev -> KeyPress (parseKeyCode $ KeyboardEvent.code ev) ev
    , onKeyUp \ev -> KeyRelease (parseKeyCode $ KeyboardEvent.code ev) ev
    , onWheel WheelScroll
    ]
    [ thead_
        [ tr_
            $ toArray
            $ renderHeaderCorner
            : (renderColumnHeader selectedCell multiSelection <$> columns)
        ]
    , tbody_ $ toArray $
        do
          row <- rows
          pure $ tr_
            $ toArray
            $ renderRowHeader selectedCell multiSelection row
            :
              do
                column <- columns
                let
                  cell = { column, row }
                  cellValue = Map.lookup cell tableData
                pure $ renderBodyCell selectedCell multiSelection activeInput
                  cell
                  cellValue
    ]

renderRowHeader :: forall i. Cell -> MultiSelection -> Row -> HTML i Action
renderRowHeader selected selection row =
  th
    [ id $ show row
    , tabIndex 0
    , classes $ [ rowHeader ]
        <>? isRowSelected selected selection row
        /\ selectedHeader
    , draggable true
    , onClick $ ClickHeader $ RowHeader row
    , onMouseDown $ HoverHeader Start $ RowHeader row
    , onMouseUp $ HoverHeader End $ RowHeader row
    , onMouseOver $ HoverHeader Over $ RowHeader row
    , onDragStart $ DragHeader Start $ RowHeader row
    , onDrop $ DragHeader End $ RowHeader row
    , onDragOver $ DragHeader Over $ RowHeader row
    ]
    [ text $ show row ]

renderColumnHeader
  :: forall i. Cell -> MultiSelection -> Column -> HTML i Action
renderColumnHeader selected selection column =
  th
    [ id $ show column
    , tabIndex 0
    , classes $ [ columnHeader ]
        <>? isColumnSelected selected selection column
        /\ selectedHeader
    , draggable true
    , onClick $ ClickHeader $ ColumnHeader column
    , onMouseDown $ HoverHeader Start $ ColumnHeader column
    , onMouseUp $ HoverHeader End $ ColumnHeader column
    , onMouseOver $ HoverHeader Over $ ColumnHeader column
    , onDragStart $ DragHeader Start $ ColumnHeader column
    , onDrop $ DragHeader End $ ColumnHeader column
    , onDragOver $ DragHeader Over $ ColumnHeader column
    ]
    [ text $ show column ]

renderHeaderCorner :: forall i. HTML i Action
renderHeaderCorner =
  th
    [ class_ cornerHeader
    , tabIndex 0
    , onClick $ ClickHeader CornerHeader
    ]
    [ text mempty ]

renderBodyCell
  :: forall i
   . Cell
  -> MultiSelection
  -> Boolean
  -> Cell
  -> Maybe CellValue
  -> HTML i Action
renderBodyCell selected selection active cell cellValue =
  td
    [ id $ showCell cell
    , tabIndex 0
    , classes $ bodyCellSelectionClasses selected selection cell
    , onClick $ ClickCell cell
    , onDoubleClick $ DoubleClickCell cell
    , onMouseDown $ HoverCell Start cell
    , onMouseUp $ HoverCell End cell
    , onMouseOver $ HoverCell Over cell
    ]
    [ input
        [ type_ InputText
        , tabIndex 0
        , autocomplete AutocompleteOff
        , disabled $ not $ cell == selected && active
        , value $ foldMap show cellValue
        , onValueChange $ WriteCell cell <<< parseCellValue
        , onKeyDown \ev -> InputKeyPress (parseKeyCode $ KeyboardEvent.code ev)
            ev
        ]
    ]

bodyCellSelectionClasses :: Cell -> MultiSelection -> Cell -> Array ClassName
bodyCellSelectionClasses selected selection cell =
  [ sheetCell ]
    <>? (selected == cell)
    /\ selectedSheetCell
    <>? isCellInSelection selection cell
    /\ inSelection
    <>? isCellAboveSelection selection cell
    /\ aboveSelection
    <>? isCellBelowSelection selection cell
    /\ belowSelection
    <>? isCellAtLeftSelection selection cell
    /\ atLeftSelection
    <>? isCellAtRightSelection selection cell
    /\ atRightSelection
