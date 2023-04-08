module App.Components.Table where

import Prelude

import App.CSS.Table (strippedTable)
import App.Utils.ArrayUtils ((..))

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))



_headerCell = Proxy :: Proxy "headerCell"
_bodyCell = Proxy :: Proxy "bodyCell"


newtype Column = Column Char
derive newtype instance eqColumn :: Eq Column
derive newtype instance ordColumn :: Ord Column

newtype Row = Row Int
derive newtype instance eqRow :: Eq Row
derive newtype instance ordRow :: Ord Row


instance Show Column where
  show (Column x) = fromCharArray [x]

instance Show Row where
  show (Row x) = show x


type Cell = { column :: Column, row :: Row }


showCell :: Cell -> String
showCell { column, row } = show column <> show row


type State
  = { selectedCell :: Cell,
      tableData :: Map Cell CellValue,
      columns :: Array Column,
      rows :: Array Row }

data CellValue =
  StringVal String |
  IntVal Int

instance Show CellValue where
  show (StringVal str) = str
  show (IntVal int) = show int


component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval
    }


initialState :: State
initialState = { selectedCell: { column: Column 'A', row: Row 1 },
                 tableData: Map.fromFoldable [
                   { column: Column 'A', row: Row 1 } /\ StringVal "value"],
                 columns: Column <$> 'A' .. 'Z',
                 rows: Row <$> 1 .. 30 }


renderHeaderCell :: forall i o. Column -> HH.HTML i o
renderHeaderCell column =
  HH.th
  [ HP.id   $ show column ]
  [ HH.text $ show column ]


renderBodyCell :: forall i o. Cell -> Maybe CellValue -> HH.HTML i o
renderBodyCell cell value =
  HH.td
  [ HP.id   $ show cell ]
  [ HH.text $ fromMaybe "" $ show <$> value ]


render :: forall ac cs m. State -> H.ComponentHTML ac cs m
render state = HH.table
           [ HP.class_ strippedTable ]
           [ HH.thead_ [
                HH.tr_ $
                  renderHeaderCell <$> state.columns
            ],
             HH.tbody_ $
               do row <- state.rows
                  pure $ HH.tr_ $
                    do column <- state.columns
                       let cell = { column, row }
                       pure $ renderBodyCell cell $ Map.lookup cell state.tableData
           ]
