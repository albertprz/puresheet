module App.Utils.Common where

import Prelude

import Debug (class DebugWarning, spyWith)

spyShow :: DebugWarning => forall a. Show a => String -> a -> a
spyShow msg = spyWith msg show

foreign import refEquals :: forall a. a -> a -> Boolean
