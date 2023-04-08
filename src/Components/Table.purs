module App.Components.Table where

import Prelude

import App.CSS.Table (strippedTable)
import Data.Array ((..))
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))


_headerCell = Proxy :: Proxy "headerCell"
_bodyCell = Proxy :: Proxy "bodyCell"


data Column = Column Char
derive instance eqColumn :: Eq Column
derive instance ordColumn :: Ord Column

data Row = Row Int
derive instance eqRow :: Eq Row
derive instance ordRow :: Ord Row


instance Show Column where
  show (Column x) = fromCharArray [x]

instance Show Row where
  show (Row x) = show x


type Cell = { column :: Column, row :: Row }

type State
  = { selectedCell :: Cell,
      tableData :: Map Cell String,
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
                   { column: Column 'A', row: Row 1 } /\ "value"],
                 columns: Column <$> ['A', 'B', 'C', 'D', 'E'],
                 rows: Row <$> 1 .. 30 }


renderHeaderCell :: forall i o. Column -> HH.HTML i o
renderHeaderCell column =
  HH.th_ [ HH.text $ show column ]


renderBodyCell :: forall i o. CellValue -> HH.HTML i o
renderBodyCell value =
  HH.td_ [ HH.text $ show value ]


render :: forall ac cs m. State -> H.ComponentHTML ac cs m
render state = HH.table
           [ HP.class_ strippedTable ]
           [ HH.thead_ [
                HH.tr_ $
                  renderHeaderCell <$> state.columns
            ]
          ]
