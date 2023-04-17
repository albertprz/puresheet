module App.Utils.CharUtils where

import Prelude

import App.Utils.ArrayUtils ((..))
import App.Utils.NumberUtils (dec, inc)
import Data.Array.NonEmpty (elem)
import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (fromMaybe)

nextChar :: Char -> Char
nextChar = fromMaybe '?' <<< fromCharCode <<< inc <<< toCharCode

prevChar :: Char -> Char
prevChar = fromMaybe '?' <<< fromCharCode <<< dec <<< toCharCode

isAplha :: Char -> Boolean
isAplha ch = isLower ch || isUpper ch

isLower :: Char -> Boolean
isLower ch = elem ch $ 'a' .. 'z'

isUpper :: Char -> Boolean
isUpper ch = elem ch $ 'A' .. 'Z'
