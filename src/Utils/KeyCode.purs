module App.Utils.KeyCode where

import FatPrelude

import App.Utils.String (last, startsWith) as String
import Data.Int as Int
import Data.String.CodeUnits (takeRight) as String
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data KeyCode
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Enter
  | Tab
  | Space
  | Delete
  | Shift
  | Control
  | Comma
  | CharKeyCode Char
  | DigitKeyCode Int
  | OtherKeyCode String

derive instance Eq KeyCode

mkKeyAction :: forall a. (KeyCode -> KeyboardEvent -> a) -> KeyboardEvent -> a
mkKeyAction ctor ev =
  ctor (parseKeyCode $ KeyboardEvent.code ev) ev

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
parseKeyCode "ControlLeft" = Control
parseKeyCode "ControlRight" = Control
parseKeyCode "MetaLeft" = Control
parseKeyCode "MetaRight" = Control
parseKeyCode "Comma" = Comma
parseKeyCode str
  | String.startsWith "Key" str
  , Just ch <- String.last str = CharKeyCode ch
parseKeyCode str
  | String.startsWith "Digit" str
  , Just n <- Int.fromString $ String.takeRight 1 str = DigitKeyCode n
parseKeyCode str = OtherKeyCode str

isModifierKeyCode :: KeyCode -> Boolean
isModifierKeyCode = flip elem [ Control, Shift ]
