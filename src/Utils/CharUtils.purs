module App.Utils.CharUtils where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Char (fromCharCode, toCharCode)

nextChar :: Char -> Char
nextChar = fromMaybe '?' <<< fromCharCode <<< (_ + 1) <<< toCharCode

prevChar :: Char -> Char
prevChar = fromMaybe '?' <<< fromCharCode <<< (_ - 1) <<< toCharCode
