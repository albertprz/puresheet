module App.Utils.Monoid where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

infixl 5 whenMonoidAppend as <>?

whenMonoid :: forall m. Monoid m => Boolean -> m -> m
whenMonoid cond m = if cond then m else mempty

unlessMonoid :: forall m. Monoid m => Boolean -> m -> m
unlessMonoid = whenMonoid <<< not

whenPlus :: forall m a. Plus m => Boolean -> m a -> m a
whenPlus cond m = if cond then m else empty

unlessPlus :: forall m a. Plus m => Boolean -> m a -> m a
unlessPlus = whenPlus <<< not

whenMonoidAppend
  :: forall f a
   . Monoid (f a)
  => Applicative f
  => f a
  -> Tuple Boolean a
  -> f a
whenMonoidAppend m1 (cond /\ m2) = m1 <> whenMonoid cond (pure m2)
