module App.Components.Table where

import FatPrelude
import Prim hiding (Row)

import App.CSS.Table (strippedTable)
import App.Utils.ArrayUtils (nextChar, prevChar)
import DOM.HTML.Indexed.AutocompleteType (AutocompleteType(..))
import Data.Int as Int
import Data.Map as Map
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

_headerCell = Proxy :: Proxy "headerCell"
_bodyCell = Proxy :: Proxy "bodyCell"

newtype Column = Column Char

derive newtype instance eqColumn :: Eq Column
derive newtype instance ordColumn :: Ord Column

newtype Row = Row Int

derive newtype instance eqRow :: Eq Row
derive newtype instance ordRow :: Ord Row

instance Show Column where
  show (Column x) = fromCharArray [ x ]

instance Show Row where
  show (Row x) = show x

type Cell = { column :: Column, row :: Row }

data Action
  = WriteCell Cell CellValue
  | KeyPress String KeyboardEvent
  | SelectCell (Cell -> Cell)

type State =
  { selectedCell :: Cell
  , activeInput :: Boolean
  , tableData :: Map Cell CellValue
  , columns :: Array Column
  , rows :: Array Row
  }

data CellValue
  = StringVal String
  | IntVal Int

instance Show CellValue where
  show (StringVal str) = str
  show (IntVal int) = show int

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall a. a -> State
initialState = const
  { selectedCell: { column: Column 'A', row: Row 1 }
  , activeInput: false
  , tableData: Map.fromFoldable
      [ { column: Column 'A', row: Row 1 } /\ StringVal "value",
        { column: Column 'D', row: Row 1 } /\ StringVal "another value"]
  , columns: Column <$> 'A' .. 'Z'
  , rows: Row <$> 1 .. 30
  }

showCell :: Cell -> String
showCell { column, row } = show column <> show row

nextColumn :: Column -> Column
nextColumn (Column ch) = Column $ nextChar ch

prevColumn :: Column -> Column
prevColumn (Column ch) = Column $ prevChar ch

nextCell :: Cell -> Cell
nextCell cell = cell { column = nextColumn cell.column }

prevCell :: Cell -> Cell
prevCell cell = cell { column = prevColumn cell.column }

parseCellValue :: String -> CellValue
parseCellValue str =
  fromMaybe (StringVal str) (IntVal <$> Int.fromString str)

renderHeaderCell :: forall i o. Column -> HH.HTML i o
renderHeaderCell column =
  HH.th
    [ HP.id $ show column ]
    [ HH.text $ show column ]

renderBodyCell :: forall i. Boolean -> Cell -> Maybe CellValue -> HH.HTML i Action
renderBodyCell active cell value =
  HH.td
    [ HP.id $ showCell cell, HP.tabIndex 0 ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.autocomplete AutocompleteOff
        , HP.disabled $ not active
        , HP.autofocus true
        , HP.name $ show cell
        , HP.value $ fromMaybe "" $ show <$> value
        , HE.onValueChange $ WriteCell cell <<< parseCellValue
        ]
    ]

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st = HH.table
  [ HP.class_ strippedTable, HE.onKeyDown \ev -> KeyPress (code ev) ev ]
  [ HH.thead_
      [ HH.tr_
          $ renderHeaderCell
          <$> st.columns
      ]
  , HH.tbody_ $
      do
        row <- st.rows
        pure $ HH.tr_ $
          do
            column <- st.columns
            let
              cell = { column, row }
              value = Map.lookup cell st.tableData
              active = cell == st.selectedCell && st.activeInput
            pure $ renderBodyCell active cell value
  ]

handleAction :: forall slots o m. MonadAff m
                => Action
                -> H.HalogenM State Action slots o m Unit

handleAction (WriteCell cell value) =
  H.modify_ \st -> st
    { tableData = Map.insert cell value st.tableData
    , activeInput = false
    }

handleAction (KeyPress "Tab" ev) = do
  liftEffect $ preventDefault $ KeyboardEvent.toEvent ev
  handleAction $ SelectCell nextCell

handleAction (KeyPress _ _) =
  H.modify_ \st -> st
    { activeInput = true
    }

handleAction (SelectCell cellFn) = do
  cell <- H.gets \st -> cellFn st.selectedCell
  H.modify_ \st -> st
    { selectedCell = cell,
      activeInput = false
    }
  element <- H.liftAff $ HA.selectElement $ QuerySelector $ "td#" <> showCell cell
  H.liftEffect $ fromMaybe (pure unit) (focus <$> element)
