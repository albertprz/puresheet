module App.Utils.Char where

import Prelude

import App.Utils.Maybe (unsafeFromJust)
import App.Utils.Number (dec, inc)
import Data.Argonaut (Json, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either, note)
import Data.String.CodeUnits as CodeUnits

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

decodeChar :: Json -> Either JsonDecodeError Char
decodeChar json =
  note (Named "Char" $ UnexpectedValue json)
    =<< map CodeUnits.toChar (decodeString json)
