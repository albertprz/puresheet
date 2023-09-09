module App.Utils.Common where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (class DebugWarning, spy)
import Effect.Exception (catchException)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

partialMaybe :: forall a b. (Partial => a -> b) -> a -> Maybe b
partialMaybe f a = unsafePerformEffect $ catchException (const $ pure Nothing)
  (Just <<< unsafePartial f <$> pure a)

spyShow :: DebugWarning => forall a. Show a => a -> a
spyShow x = spy (show x) x
