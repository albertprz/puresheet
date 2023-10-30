module App.Utils.Monoid where

import Prelude

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

infixl 5 whenMonoidAppend as <>?

whenMonoid :: forall m. Monoid m => Boolean -> m -> m
whenMonoid cond m = if cond then m else mempty

whenMonoidAppend
  :: forall f a
   . Monoid (f a)
  => Applicative f
  => f a
  -> Tuple Boolean a
  -> f a
whenMonoidAppend m1 (cond /\ m2) = m1 <> whenMonoid cond (pure m2)
