module App.Utils.CharUtils where

import Prelude
import App.Utils.IntUtils (dec, inc)
import Data.Maybe (fromMaybe)
import Data.Char (fromCharCode, toCharCode)

nextChar :: Char -> Char
nextChar = fromMaybe '?' <<< fromCharCode <<< inc <<< toCharCode

prevChar :: Char -> Char
prevChar = fromMaybe '?' <<< fromCharCode <<< dec <<< toCharCode
