module App.Utils.String where

import Prelude

import Data.Foldable (foldMap, intercalate)
import Data.Maybe (Maybe)
import Data.String (length)
import Data.String.CodeUnits (charAt, dropRight, uncons)

newline :: String
newline = "\n"

tab :: String
tab = "\t"

str :: forall a. Show a => String -> Array a -> String
str sep xs = intercalate sep $ show <$> xs

wrap :: String -> String -> String -> String
wrap beg end x = beg <> x <> end

wrapBoth :: String -> String -> String
wrapBoth x = wrap x x

wrapQuotes :: String -> String
wrapQuotes = wrapBoth "'"

wrapDoubleQuotes :: String -> String
wrapDoubleQuotes = wrapBoth "\""

wrapParens :: String -> String
wrapParens = wrap "(" ")"

wrapSquare :: String -> String
wrapSquare = wrap "[" "]"

showParensCsv :: forall a. Show a => Array a -> String
showParensCsv = wrapParens <<< str ", "

head :: String -> Maybe Char
head = map _.head <<< uncons

tail :: String -> String
tail = foldMap _.tail <<< uncons

last :: String -> Maybe Char
last s = charAt (length s - 1) s

init :: String -> String
init = dropRight 1

foreign import startsWith :: String -> String -> Boolean

foreign import endsWith :: String -> String -> Boolean
