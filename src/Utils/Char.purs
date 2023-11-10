module App.Utils.Char where

import Prelude

import App.Utils.Maybe (unsafeFromJust)
import App.Utils.Number (dec, inc)
import Data.Char (fromCharCode, toCharCode)

nextChar :: Char -> Char
nextChar = unsafeFromJust <<< fromCharCode <<< inc <<< toCharCode

prevChar :: Char -> Char
prevChar = unsafeFromJust <<< fromCharCode <<< dec <<< toCharCode

upperStartCode :: Int
upperStartCode = toCharCode 'A'

upperEndCode :: Int
upperEndCode = toCharCode 'Z'

toUpper :: Int -> Char
toUpper n = unsafeFromJust $ fromCharCode (upperStartCode + n)

fromUpper :: Char -> Int
fromUpper ch = toCharCode ch - upperStartCode
