module App.Components.Table.Renderer where

import FatPrelude hiding (div, span)
import Prim hiding (Row)

import App.CSS.ClassNames (aboveSelection, atLeftSelection, atRightSelection, belowSelection, columnHeader, copySelection, cornerHeader, formulaBox, formulaBoxContainer, formulaCellInput, formulaContainer, formulaSignature, inSelection, mainContainer, rowHeader, selectedCellInput, selectedHeader, selectedSheetCell, sheetCell)
import App.CSS.Ids (cellId, formulaBoxId, formulaCellInputId, formulaSignatureId, selectedCellInputId)
import App.Components.Table.Cell (Cell, CellValue, Column, Header(..), Row, cellParser, parseCellValue, showCell)
import App.Components.Table.Formula (formulaStateToClass)
import App.Components.Table.Models (Action(..), AppState, EventTransition(..))
import App.Components.Table.Selection (SelectionState(..), isCellAboveSelection, isCellAtLeftSelection, isCellAtRightSelection, isCellBelowSelection, isCellInSelection, isColumnSelected, isRowSelected)
import App.Utils.Dom (formulaElements, mkKeyAction)
import App.Utils.Map (lookup2) as Map
import Bookhound.Parser (runParser)
import Data.Map (lookup) as Map
import Halogen.HTML (ClassName, ComponentHTML, HTML, div, input, table, tbody_, td, text, th, thead_, tr_)
import Halogen.HTML.Events (onClick, onDoubleClick, onDragOver, onDragStart, onDrop, onFocusIn, onKeyDown, onKeyUp, onMouseDown, onMouseOver, onMouseUp, onValueChange, onWheel)
import Halogen.HTML.Properties (AutocompleteType(..), InputType(..), autocomplete, class_, classes, draggable, id, readOnly, style, tabIndex, type_, value)

render :: forall cs m. AppState -> ComponentHTML Action cs m
render
  st@
    { selectedCell
    , formulaCell
    , activeFormula
    , formulaState
    , selectionState
    } =
  div [ class_ mainContainer ]
    [ div [ class_ formulaContainer ]
        [ input
            [ id $ show selectedCellInputId
            , tabIndex zero
            , classes [ selectedCellInput ]
            , value $ showCell selectedCell
            , onValueChange $ WriteSelectedCellInput <<< parseCell
            , onKeyDown $ mkKeyAction SelectedCellInputKeyDown
            ]
        , div
            [ class_ formulaBoxContainer ]
            [ div
                [ id $ show formulaBoxId
                , tabIndex zero
                , classes [ formulaBox, formulaStateToClass formulaState ]
                , onKeyDown $ mkKeyAction FormulaKeyDown
                , onKeyUp $ mkKeyAction FormulaKeyUp
                , onFocusIn FocusInFormula
                ]
                (renderFormulaDisplay st)
            , div
                [ id $ show formulaSignatureId
                , classes [ formulaSignature ]
                ]
                [ text mempty ]
            ]
        , input
            [ id $ show formulaCellInputId
            , tabIndex zero
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
        , onKeyUp $ mkKeyAction KeyRelease
        , onWheel WheelScroll
        ]
        [ renderHeader st
        , renderBody st
        ]
    ]
  where
  parseCell = hush <<< runParser cellParser

renderFormulaDisplay :: forall i. AppState -> Array (HTML i Action)
renderFormulaDisplay { selectedCell, formulaCache, tableFormulas } =
  formulaElements
    $ foldMap _.formulaText
    $ Map.lookup2 selectedCell
        formulaCache
        tableFormulas

renderHeader :: forall i. AppState -> HTML i Action
renderHeader st@{ columns } =
  thead_
    [ tr_
        $ toArray
        $ cons
            renderHeaderCorner
            (renderColumnHeader st <$> columns)
    ]
  where
  renderHeaderCorner =
    th
      [ class_ cornerHeader
      , tabIndex zero
      , onClick $ ClickHeader CornerHeader
      ]
      [ text mempty ]

renderBody :: forall i. AppState -> HTML i Action
renderBody
  st@
    { rows
    , columns
    , tableData
    } =
  tbody_ $ toArray do
    row <- rows
    pure $ tr_ $ toArray $ cons
      (renderRowHeader st row)
      (renderRow row)
  where
  renderRow row = do
    column <- columns
    let
      cell = { column, row }
      cellValue = Map.lookup cell tableData
    pure $ renderBodyCell st cell cellValue

renderRowHeader :: forall i. AppState -> Row -> HTML i Action
renderRowHeader { selectedCell, multiSelection } row =
  th
    [ id $ show row
    , tabIndex zero
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
  :: forall i. AppState -> Column -> HTML i Action
renderColumnHeader { selectedCell, multiSelection } column =
  th
    [ id $ show column
    , tabIndex zero
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
   . AppState
  -> Cell
  -> Maybe CellValue
  -> HTML i Action
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
        , tabIndex zero
        , autocomplete AutocompleteOff
        , readOnly $ not $ cell == selectedCell && activeInput
        , value $ foldMap show cellValue
        , onValueChange $ WriteCell cell <<< parseCellValue
        ]
    ]

bodyCellSelectionClasses :: AppState -> Cell -> Array ClassName
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
