module App.Utils.MonoidUtils where

import Prelude

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

whenMonoid :: forall m. Monoid m => Boolean -> m -> m
whenMonoid cond m = if cond then m else mempty

infixl 5 whenMonoidAppend as <>!

whenMonoidAppend
  :: forall f a
   . Monoid (f a)
  => Applicative f
  => f a
  -> Tuple Boolean a
  -> f a
whenMonoidAppend m1 (cond /\ m2) = m1 <> whenMonoid cond (pure m2)
