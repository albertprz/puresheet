module App.Utils.Common where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Exception (catchException)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

partialMaybe :: forall a b. (Partial => a -> b) -> a -> Maybe b
partialMaybe f a = unsafePerformEffect $ catchException (const $ pure Nothing)
  (Just <<< unsafePartial f <$> pure a)
