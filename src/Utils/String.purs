module App.Utils.String where

import Prelude

import App.Utils.Monoid (unlessMonoid)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe)
import Data.String (null) as String
import Data.String.CodeUnits (take, takeRight, toChar)

newline :: String
newline = "\n"

tab :: String
tab = "\t"

str :: forall a. Show a => String -> Array a -> String
str sep xs = intercalate sep $ show <$> xs

wrap :: String -> String -> String -> String
wrap beg end x = unlessMonoid (String.null x) $ beg <> x <> end

wrapBoth :: String -> String -> String
wrapBoth x = wrap x x

wrapCurly :: String -> String
wrapCurly = wrap "{" "}"

wrapQuotes :: String -> String
wrapQuotes = wrapBoth "'"

wrapDoubleQuotes :: String -> String
wrapDoubleQuotes = wrapBoth "\""

wrapBackQuotes :: String -> String
wrapBackQuotes = wrapBoth "`"

wrapParens :: String -> String
wrapParens = wrap "(" ")"

showParensCsv :: forall a. Show a => Array a -> String
showParensCsv = wrapParens <<< str ", "

head :: String -> Maybe Char
head = toChar <<< take 1

last :: String -> Maybe Char
last = toChar <<< takeRight 1

foreign import startsWith :: String -> String -> Boolean

foreign import endsWith :: String -> String -> Boolean
