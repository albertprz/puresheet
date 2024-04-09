module App.Components.Table.Renderer where

import FatPrelude hiding (div)
import Prim hiding (Row)

import App.CSS.ClassNames (aboveSelection, atLeftSelection, atRightSelection, belowSelection, columnHeader, copySelection, cornerHeader, formulaCellInput, formulaSectionContainer, inSelection, mainContainer, rowHeader, selectedCellInput, selectedHeader, selectedSheetCell, sheetCell)
import App.CSS.Ids (cellId, formulaCellInputId, selectedCellInputId)
import App.Components.Editor (EditorSlot, _editor)
import App.Components.Editor as Editor
import App.Components.Table.Cell (Cell, CellValue, Column, Header(..), Row, allColumns, cellParser, parseCellValue, showCell)
import App.Components.Table.Handler (handleEditorOutput)
import App.Components.Table.Models (EventTransition(..), TableAction(..), TableState)
import App.Components.Table.Selection (SelectionState(..), isCellAboveSelection, isCellAtLeftSelection, isCellAtRightSelection, isCellBelowSelection, isCellInSelection, isColumnSelected, isRowSelected)
import App.Utils.KeyCode (mkKeyAction)
import Bookhound.Parser (runParser)
import CSSPrelude (ComponentHTML)
import Data.Array as Array
import Data.HashMap as HashMap
import Halogen.HTML (ClassName, HTML, div, input, slot, table, tbody_, td, text, th, thead_, tr_)
import Halogen.HTML.Events (onClick, onDoubleClick, onDragOver, onDragStart, onDrop, onFocusIn, onKeyDown, onKeyUp, onMouseDown, onMouseOver, onMouseUp, onValueChange, onWheel)
import Halogen.HTML.Properties (AutocompleteType(..), InputType(..), autocomplete, class_, classes, draggable, id, readOnly, style, tabIndex, type_, value)

render
  :: forall m
   . MonadAff m
  => TableState
  -> ComponentHTML TableAction Slots m
render
  st@
    { selectedCell
    , formulaCell
    , activeFormula
    , selectionState
    , formulaState
    } =
  div [ class_ mainContainer ]
    [ div [ class_ formulaSectionContainer ]
        [ input
            [ id $ show selectedCellInputId
            , classes [ selectedCellInput ]
            , value $ showCell selectedCell
            , onValueChange $ WriteSelectedCellInput <<< parseCell
            , onKeyDown $ mkKeyAction SelectedCellInputKeyDown
            ]
        , slot _editor unit Editor.component { formulaState } handleEditorOutput
        , input
            [ id $ show formulaCellInputId
            , class_ formulaCellInput
            , type_ $ if activeFormula then InputText else InputHidden
            , value $ showCell formulaCell
            , onValueChange $ WriteFormulaCellInput <<< parseCell
            , onKeyDown $ mkKeyAction FormulaCellInputKeyDown
            ]
        ]
    , table
        [ classes $ whenMonoid (selectionState == CopySelection)
            [ copySelection ]
        , style "border-spacing: 0"
        , onKeyDown $ mkKeyAction KeyDown
        , onKeyUp $ mkKeyAction KeyUp
        , onWheel WheelScroll
        ]
        [ renderHeader st
        , renderBody st
        ]
    ]
  where
  parseCell = hush <<< runParser cellParser

renderHeader :: forall i. TableState -> HTML i TableAction
renderHeader st =
  thead_
    [ tr_
        $ toArray
        $ cons
            ( th
                [ class_ cornerHeader
                , onClick $ ClickHeader CornerHeader
                ]
                [ text mempty ]
            )
            (renderColumnHeader st <$> allColumns)
    ]

renderBody :: forall i. TableState -> HTML i TableAction
renderBody
  st@
    { rows
    , tableData
    } =
  tbody_ do
    row <- toArray rows
    pure $ tr_ $ Array.cons
      (renderRowHeader st row)
      (renderRow row)
  where
  renderRow row = do
    column <- toArray allColumns
    let
      cell = { column, row }
      cellValue = HashMap.lookup cell tableData
    pure $ renderBodyCell st cell cellValue

renderRowHeader :: forall i. TableState -> Row -> HTML i TableAction
renderRowHeader { selectedCell, multiSelection } row =
  th
    [ id $ show row
    , classes $ [ rowHeader ]
        <>? isRowSelected selectedCell multiSelection row
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
  :: forall i. TableState -> Column -> HTML i TableAction
renderColumnHeader { selectedCell, multiSelection } column =
  th
    [ id $ show column
    , classes $ [ columnHeader ]
        <>? isColumnSelected selectedCell multiSelection column
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

renderBodyCell
  :: forall i
   . TableState
  -> Cell
  -> Maybe CellValue
  -> HTML i TableAction
renderBodyCell st@{ selectedCell, activeInput } cell cellValue =
  td
    [ id $ show cellId <> showCell cell
    , tabIndex zero
    , classes $ bodyCellSelectionClasses st cell
    , onClick $ ClickCell cell
    , onDoubleClick $ DoubleClickCell cell
    , onMouseDown $ HoverCell Start cell
    , onMouseUp $ HoverCell End cell
    , onMouseOver $ HoverCell Over cell
    , onFocusIn $ FocusInCell cell
    ]
    [ input
        [ type_ InputText
        , autocomplete AutocompleteOff
        , readOnly $ not $ cell == selectedCell && activeInput
        , value $ foldMap show cellValue
        , onValueChange $ WriteCell cell <<< parseCellValue
        ]
    ]

bodyCellSelectionClasses :: TableState -> Cell -> Array ClassName
bodyCellSelectionClasses { selectedCell, multiSelection } cell =
  [ sheetCell ]
    <>? (selectedCell == cell)
    /\ selectedSheetCell
    <>? isCellInSelection multiSelection cell
    /\ inSelection
    <>? isCellAboveSelection multiSelection cell
    /\ aboveSelection
    <>? isCellBelowSelection multiSelection cell
    /\ belowSelection
    <>? isCellAtLeftSelection multiSelection cell
    /\ atLeftSelection
    <>? isCellAtRightSelection multiSelection cell
    /\ atRightSelection

type Slots = (editor :: EditorSlot)
