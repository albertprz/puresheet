module App.Utils.String where

import Prelude

import App.Utils.Monoid (whenMonoid)
import Data.String as String

newline :: String
newline = "\n"

tab :: String
tab = "\t"

wrap :: String -> String -> String -> String
wrap beg end x = whenMonoid (not $ String.null x) $ beg <> x <> end

wrapBoth :: String -> String -> String
wrapBoth x = wrap x x

wrapCurly :: String -> String
wrapCurly = wrap "{" "}"

wrapQuotes :: String -> String
wrapQuotes = wrapBoth "'"

wrapBackQuotes :: String -> String
wrapBackQuotes = wrapBoth "`"
