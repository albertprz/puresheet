module CSSPrelude
  ( module Prelude
  , module Tuple.Nested
  , module Tecton
  , module Common
  , module Halogen
  , module Tecton.Halogen
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, clamp, compare, comparing, compose, conj, const, degree, disj, eq, flap, flip, gcd, identity, ifM, lcm, liftA1, liftM1, map, mempty, mod, mul, negate, notEq, one, otherwise, pure, recip, show, unit, unless, unlessM, void, when, whenM, zero, discard, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||))
import Data.Tuple.Nested ((/\)) as Tuple.Nested
import Halogen (AttrName(..), ClassName(..), Component, ComponentHTML, ElemName(..), PropName(..), RefLabel(..))
import Tecton hiding ((&.), style)
import Tecton.Halogen (style, styleSheet, (&.))
import App.CSS.Common (black, darkGrey, green, grey, hex, lightGreen, lightGrey, lighterGrey, red, white) as Common
