module App.Components.Table.Renderer where

import FatPrelude hiding (div)
import Prim hiding (Row)

import App.CSS.ClassNames (aboveSelection, atLeftSelection, atRightSelection, belowSelection, columnHeader, copySelection, cornerHeader, formulaBox, formulaCellInput, formulaContainer, inSelection, mainContainer, rowHeader, selectedCellInput, selectedHeader, selectedSheetCell, sheetCell)
import App.CSS.Ids (cellId, formulaBoxId, formulaCellInputId, selectedCellInputId)
import App.Components.Table.Cell (Cell, CellValue, Column, Header(..), MultiSelection, Row, SelectionState(..), isCellAboveSelection, isCellAtLeftSelection, isCellAtRightSelection, isCellBelowSelection, isCellInSelection, isColumnSelected, isRowSelected, parseCell, parseCellValue, showCell)
import App.Components.Table.Formula (formulaStateToClass)
import App.Components.Table.Models (Action(..), AppState, EventTransition(..))
import App.Utils.Dom (parseKeyCode)
import App.Utils.Map (lookup2) as Map
import Data.Map (lookup) as Map
import Halogen.HTML (ClassName, ComponentHTML, HTML, div, input, table, tbody_, td, text, textarea, th, thead_, tr_)
import Halogen.HTML.Events (onClick, onDoubleClick, onDragOver, onDragStart, onDrop, onFocusIn, onKeyDown, onKeyUp, onMouseDown, onMouseOver, onMouseUp, onValueChange, onWheel)
import Halogen.HTML.Properties (AutocompleteType(..), InputType(..), autocomplete, class_, classes, draggable, id, readOnly, style, tabIndex, type_, value)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

render :: forall cs m. AppState -> ComponentHTML Action cs m
render
  { selectedCell
  , formulaCell
  , activeFormula
  , activeInput
  , formulaState
  , tableData
  , tableFormulas
  , formulaCache
  , columns
  , rows
  , multiSelection
  , selectionState
  } =
  div [ class_ mainContainer ]
    [ div [ class_ formulaContainer ]
        [ input
            [ id selectedCellInputId
            , tabIndex 0
            , classes [ selectedCellInput ]
            , value $ showCell selectedCell
            , onValueChange $ WriteSelectedCellInput <<< parseCell
            , onKeyDown \ev -> SelectedCellInputKeyPress
                (parseKeyCode $ KeyboardEvent.code ev)
                ev
            ]
        , textarea
            [ id formulaBoxId
            , tabIndex 0
            , classes [ formulaBox, formulaStateToClass formulaState ]
            , style "resize: none"
            , value $ foldMap _.formulaText $ Map.lookup2 selectedCell
                formulaCache
                tableFormulas
            , onKeyDown \ev -> FormulaKeyPress
                (parseKeyCode $ KeyboardEvent.code ev)
                ev
            , onFocusIn FocusInFormula
            ]
        , input
            [ id formulaCellInputId
            , tabIndex 0
            , classes [ formulaCellInput ]
            , type_ $ if activeFormula then InputText else InputHidden
            , value $ showCell formulaCell
            , onValueChange $ WriteFormulaCellInput <<< parseCell
            , onKeyDown \ev -> FormulaCellInputKeyPress
                (parseKeyCode $ KeyboardEvent.code ev)
                ev
            ]
        ]
    , table
        [ classes $ whenMonoid (selectionState == CopySelection)
            [ copySelection ]
        , style "border-spacing: 0"
        , onKeyDown \ev -> KeyPress (parseKeyCode $ KeyboardEvent.code ev) ev
        , onKeyUp \ev -> KeyRelease (parseKeyCode $ KeyboardEvent.code ev) ev
        , onWheel WheelScroll
        ]
        [ thead_
            [ tr_
                $ toArray
                $ cons renderHeaderCorner
                    (renderColumnHeader selectedCell multiSelection <$> columns)
            ]
        , tbody_ $ toArray $
            do
              row <- rows
              pure $ tr_
                $ toArray
                $ cons
                    (renderRowHeader selectedCell multiSelection row)
                    ( do
                        column <- columns
                        let
                          cell = { column, row }
                          cellValue = Map.lookup cell tableData
                        pure $ renderBodyCell selectedCell multiSelection
                          activeInput
                          cell
                          cellValue
                    )
        ]
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
    , onMouseUp $ HoverHeader Over $ RowHeader row
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
    , onMouseUp $ HoverHeader Over $ ColumnHeader column
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
    [ id $ cellId <> showCell cell
    , tabIndex 0
    , classes $ bodyCellSelectionClasses selected selection cell
    , onClick $ ClickCell cell
    , onDoubleClick $ DoubleClickCell cell
    , onMouseDown $ HoverCell Start cell
    , onMouseUp $ HoverCell End cell
    , onMouseOver $ HoverCell Over cell
    , onFocusIn $ FocusInCell cell
    ]
    [ input
        [ type_ InputText
        , tabIndex 0
        , autocomplete AutocompleteOff
        , readOnly $ not $ cell == selected && active
        , value $ foldMap show cellValue
        , onValueChange $ WriteCell cell <<< parseCellValue
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
