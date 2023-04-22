module App.Components.Table.HandlerHelpers where

import FatPrelude
import Prim hiding (Row)

import App.Components.Table.Cell (Cell, CellMove, Column, MultiSelection(..), Row(..), getCellFromMove, getSelectionTargetCell, parseColumn, parseRow, showCell)
import App.Components.Table.Models (KeyCode(..), State)
import App.Utils.DomUtils (selectAllVisibleElements, selectElement)
import Data.Array as Array
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Element (id, scrollWidth)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (HTMLElement, Window, window)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Window (scrollBy)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, shiftKey)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

cellArrowMove :: forall m. MonadEffect m => MonadState State m => KeyboardEvent -> CellMove -> m Unit
cellArrowMove ev move = withPrevent ev
  if (shiftKey ev) then
    modify_ \st -> st
      { multiSelection = CellsSelection st.selectedCell
          $ getCellFromMove move st.columns st.rows
          $ fromMaybe st.selectedCell
          $ getSelectionTargetCell st.multiSelection
      }
  else
    cellMove ev move

cellMove :: forall m a. MonadEffect m => MonadState State m => IsEvent a => a -> CellMove -> m Unit
cellMove ev move = withPrevent ev do
  active <- gets \st -> st.activeInput
  when (not active) $ selectCell move

selectCell :: forall m. MonadEffect m => MonadState State m => CellMove -> m Unit
selectCell move = do
  originCell <- gets \st -> st.selectedCell
  { selectedCell, columns, rows } <- modify \st -> st
    { activeInput = false
    , multiSelection = NoSelection
    , selectedCell = getCellFromMove move st.columns st.rows st.selectedCell
    }
  visibleCols <- getVisibleCols
  visibleRows <- getVisibleRows
  goToCell visibleCols visibleRows columns rows originCell selectedCell

goToCell :: forall m. MonadEffect m => MonadState State m => Array Element -> Array Element -> NonEmptyArray Column -> NonEmptyArray Row -> Cell -> Cell -> m Unit
goToCell visibleCols visibleRows allColumns allRows origin target = do
  cols <- parseElems parseColumn visibleCols
  rows <- parseElems parseRow visibleRows
  sequence_ $ adjustRows (length visibleRows - 1) target.row <$> maximum allRows <*> minimum allRows
  liftEffect $ goToCellHelper cols rows allColumns origin target visibleCols

goToCellHelper :: Array Column -> Array Row -> NonEmptyArray Column -> Cell -> Cell -> Array Element -> Effect Unit
goToCellHelper cols rows allColumns origin { column, row } visibleCols

  | last' cols == Just column && last allColumns /= origin.column = do
      width <- traverse scrollWidth $ head' visibleCols
      scrollByX (coalesce width + 1.0) =<< window

  | head' cols == Just column && head allColumns /= origin.column = do
      width <- traverse scrollWidth $ last' visibleCols
      scrollByX (-(coalesce width + 1.0)) =<< window

  | not (elem row rows && elem column cols) =
      actOnCell { column, row } focus Nothing

  | otherwise = pure unit

adjustRows :: forall m. MonadState State m => Int -> Row -> Row -> Row -> m Unit
adjustRows rowRange (Row currentRow) (Row maxRow) (Row minRow)

  | currentRow + 1 > maxRow = modify_ \st -> st
      { rows = Row <$> (currentRow - rowRange + 1) .. (currentRow + 1) }

  | currentRow < minRow = modify_ \st -> st
      { rows = Row <$> currentRow .. (currentRow + rowRange) }

  | otherwise = pure unit

initialize :: forall m. MonadState State m => MonadEffect m => m Unit
initialize = do
  { selectedCell, rows } <- get
  let Row (firstRow) = head rows
  visibleRows <- parseElems parseRow =<< getVisibleRows
  modify_ \st -> st
    { rows = Row <$> firstRow .. (firstRow + length visibleRows - 2) }
  actOnCell selectedCell focus Nothing

parseElems :: forall m a. MonadEffect m => (String -> Maybe a) -> Array Element -> m (Array a)
parseElems f elems = liftEffect (Array.catMaybes <$> traverse ((f <$> _) <<< id) elems)

getVisibleCols :: forall m. MonadEffect m => m (Array Element)
getVisibleCols = selectAllVisibleElements $ QuerySelector "th.column-header"

getVisibleRows :: forall m. MonadEffect m => m (Array Element)
getVisibleRows = selectAllVisibleElements $ QuerySelector "th.row-header"

actOnCell :: forall m. MonadEffect m => Cell -> (HTMLElement -> Effect Unit) -> Maybe String -> m Unit
actOnCell cell action subElem = do
  element <- selectElement $ QuerySelector $ "td#"
    <> showCell cell
    <> foldMap (" " <> _) subElem
  liftEffect $ traverse_ action element

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

parseKeyCode :: String -> KeyCode
parseKeyCode "ArrowLeft" = ArrowLeft
parseKeyCode "ArrowRight" = ArrowRight
parseKeyCode "ArrowUp" = ArrowUp
parseKeyCode "ArrowDown" = ArrowDown
parseKeyCode "Enter" = Enter
parseKeyCode "Tab" = Tab
parseKeyCode "Space" = Space
parseKeyCode "Delete" = Delete
parseKeyCode "Backspace" = Delete
parseKeyCode "ShiftLeft" = Shift
parseKeyCode "ShiftRight" = Shift
parseKeyCode str = OtherKey str

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
