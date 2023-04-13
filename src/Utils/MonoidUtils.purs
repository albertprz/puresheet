module App.Utils.MonoidUtils where

import Prelude

whenMonoid :: forall m. Monoid m => Boolean -> m -> m
whenMonoid cond m = if cond then m else mempty
