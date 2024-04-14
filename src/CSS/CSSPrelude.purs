module CSSPrelude
  ( module Prelude
  , module Tuple
  , module Tuple.Nested
  , module Tecton
  , module Halogen
  , module Tecton.Internal
  , module Common
  , module ClassNames
  , module Ids
  ) where

import Tecton hiding (ElementId(..))

import App.CSS.ClassNames (aboveSelection, atLeftSelection, atRightSelection, belowSelection, cellSyntax, columnHeader, copySelection, cornerHeader, formulaBox, formulaCellInput, formulaSectionContainer, inSelection, invalidFormula, keywordSyntax, numberSyntax, operatorSyntax, regularSyntax, rowHeader, selectedCellInput, selectedHeader, selectedSheetCell, sheet, sheetCell, spreadsheetContainer, stringSyntax, strippedSheet, symbolSyntax, unknownFormula, validFormula) as ClassNames
import App.CSS.Common (black, blue, brown, darkGrey, green, grey, grey2, hex, lightGreen, lightGrey, lighterGreen, lighterGrey, lighterRed, lighterYellow, mustard, orange, pink, purple, red, white, yellow) as Common
import App.CSS.Ids (ElementId(..), cellId, formulaBoxId, formulaCellInputId, inputElement, selectedCellInputId) as Ids
import Data.Tuple (Tuple) as Tuple
import Data.Tuple.Nested ((/\)) as Tuple.Nested
import Halogen (AttrName(..), ClassName(..), Component, ComponentHTML, ElemName(..), PropName(..), RefLabel(..))
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, clamp, compare, comparing, compose, conj, const, degree, discard, disj, eq, flap, flip, gcd, identity, ifM, lcm, liftA1, liftM1, map, mempty, mod, mul, negate, notEq, one, otherwise, pure, recip, show, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||))
import Tecton.Internal (Extensible, Selector)
