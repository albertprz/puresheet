module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, Column, Row, getCell, getColumnCell, getRowCell, parseColumn, parseRow, showCell)
import App.Components.Table.Models (Action, CellMove(..), Key(..), State)
import App.Utils.DomUtils (selectAllVisibleElements, selectElement)
import App.Utils.NumberUtils (coalesce)
import Data.Array as Array
import Halogen as H
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Element (id, scrollHeight, scrollWidth)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (HTMLElement, Window, window)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Window (scrollBy)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

arrowMove
  :: forall slots o m a
   . MonadAff m
  => IsEvent a
  => a
  -> CellMove
  -> H.HalogenM State Action slots o m Unit

arrowMove ev move = withPrevent ev $ do
  active <- H.gets \st -> st.activeInput
  when (not active) $ selectCell move

selectCell :: forall slots o m. MonadAff m => CellMove -> H.HalogenM State Action slots o m Unit
selectCell move = do
  origin <- H.gets \st -> st.selectedCell
  { selectedCell, columns } <- H.modify \st -> st
    { activeInput = false
    , selectedCell = fromMaybe st.selectedCell
        $ (interpretCellMove move) st.columns st.rows st.selectedCell
    }
  visibleCols <- selectAllVisibleElements $ QuerySelector "th.column-header"
  visibleRows <- selectAllVisibleElements $ QuerySelector "th.row-header"
  goToCell visibleCols visibleRows columns origin selectedCell

goToCell :: forall m. MonadEffect m => Array Element -> Array Element -> NonEmptyArray Column -> Cell -> Cell -> m Unit
goToCell visibleCols visibleRows allColumns origin target = liftEffect $ do
  cols <- Array.catMaybes <$> traverse ((parseColumn <$> _) <<< id) visibleCols
  rows <- Array.catMaybes <$> traverse ((parseRow <$> _) <<< id) visibleRows
  goToCellHelper cols rows allColumns origin target visibleCols visibleRows

goToCellHelper :: Array Column -> Array Row -> NonEmptyArray Column -> Cell -> Cell -> Array Element -> Array Element -> Effect Unit
goToCellHelper cols rows allColumns origin { column, row } visibleCols visibleRows

  | last' cols == Just column && last allColumns /= origin.column = do
      width <- traverse scrollWidth $ head' visibleCols
      scrollByX (coalesce width + 1.0) =<< window

  | head' cols == Just column && head allColumns /= origin.column = do
      width <- traverse scrollWidth $ last' visibleCols
      scrollByX (-(coalesce width + 1.0)) =<< window

  | (last' $ rows) == Just row = do
      height <- traverse scrollHeight $ head' visibleRows
      scrollByY (coalesce height + 0.5) =<< window

  | (head' $ rows) == Just row = do
      height <- traverse scrollHeight $ last' visibleRows
      scrollByY (-(coalesce height + 0.5)) =<< window

  | not (elem row rows && elem column cols) =
      actOnCell { column, row } focus Nothing

  | otherwise = pure unit

actOnCell :: forall m. MonadEffect m => Cell -> (HTMLElement -> Effect Unit) -> Maybe String -> m Unit
actOnCell cell action subElem = do
  element <- selectElement $ QuerySelector $ "td#"
    <> showCell cell
    <> foldMap (" " <> _) subElem
  liftEffect $ traverse_ action element

interpretCellMove :: CellMove -> (NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Maybe Cell)
interpretCellMove move = case move of
  NextRow -> getRowCell inc
  PrevRow -> getRowCell dec
  NextColumn -> getColumnCell inc
  PrevColumn -> getColumnCell dec
  NextCell -> getCell inc
  PrevCell -> getCell dec
  OtherCell cell -> \_ _ _ -> Just cell

withPrevent :: forall m a b. MonadEffect m => IsEvent a => a -> m b -> m b
withPrevent ev next = prevent ev *> next

prevent :: forall m a. MonadEffect m => IsEvent a => a -> m Unit
prevent ev = liftEffect (preventDefault $ toEvent ev)

scrollByX :: Number -> Window -> Effect Unit
scrollByX x = scrollBy' x 0.0

scrollByY :: Number -> Window -> Effect Unit
scrollByY = scrollBy' 0.0

scrollBy' :: Number -> Number -> Window -> Effect Unit
scrollBy' x y = scrollBy (unsafeCoerce x) (unsafeCoerce y)

parseKey :: String -> Key
parseKey "ArrowLeft" = ArrowLeft
parseKey "ArrowRight" = ArrowRight
parseKey "ArrowUp" = ArrowUp
parseKey "ArrowDown" = ArrowDown
parseKey "Enter" = Enter
parseKey "Tab" = Tab
parseKey "Space" = Space
parseKey str = OtherKey str

class IsEvent :: forall k. k -> Constraint
class IsEvent a

instance IsEvent MouseEvent
instance IsEvent KeyboardEvent
instance IsEvent FocusEvent
instance IsEvent InputEvent
instance IsEvent DragEvent
instance IsEvent WheelEvent

toEvent :: forall a. IsEvent a => a -> Event
toEvent = unsafeCoerce
