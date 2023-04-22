module FatPrelude
  ( module Prelude
  , module MonoidUtils
  , module ArrayUtils
  , module CharUtils
  , module NumberUtils
  , module FunctorUtils
  , module Foldable
  , module Traversable
  , module Map
  , module Set
  , module Maybe
  , module Bifunctor
  , module NonEmptyArray
  , module Tuple.Nested
  , module Type.Prelude
  , module Effect
  , module Aff
  , module Unit
  , module Function
  , module Char
  , module Int
  , module CodeUnits
  , module AffClass
  , module EffClass
  , module MonadState
  , module Console
  , module PSCISupport
  ) where

import Prelude

import App.Utils.ArrayUtils (class Range, getElemSat, getNextElemSat, getPrevElemSat, head', inRange, init', last', range, satIndex, switchElements, tail', toArray', (!!!), (..)) as ArrayUtils
import App.Utils.CharUtils (isAplha, isLower, isUpper, nextChar, prevChar) as CharUtils
import App.Utils.FunctorUtils (mapp, (<$$>)) as FunctorUtils
import App.Utils.MonoidUtils (whenMonoid, whenMonoidAppend, (<>!)) as MonoidUtils
import App.Utils.NumberUtils (abs, coalesce, dec, inc, neg, pos) as NumberUtils
import Data.Array.NonEmpty hiding ((..), range, all, any, elem, notElem, find, findMap, foldM, intercalate, length, scanl, scanr) as NonEmptyArray
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Bifunctor
import Data.Char (fromCharCode, toCharCode) as Char
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, surround, surroundMap, traverse_) as Foldable
import Data.Function (applyFlipped, applyN, compose, const, flip, identity, on, (#), ($), (<<<), (>>>)) as Function
import Data.Int (Parity(..), Radix, base36, binary, ceil, decimal, even, floor, fromNumber, fromString, fromStringAs, hexadecimal, octal, odd, parity, pow, quot, radix, rem, round, toNumber, toStringAs, trunc) as Int
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Maybe
import Data.Set (Set) as Set
import Data.String.CodeUnits (fromCharArray, toCharArray) as CodeUnits
import Data.Traversable (class Traversable, Accum, mapAccumL, mapAccumR, scanl, scanr, sequence, sequenceDefault, sequence_, traverse, traverseDefault, traverse_) as Traversable
import Data.Tuple.Nested (type (/\), T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry1, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, get1, get10, get2, get3, get4, get5, get6, get7, get8, get9, over1, over10, over2, over3, over4, over5, over6, over7, over8, over9, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\)) as Tuple.Nested
import Data.Unit (Unit, unit) as Unit
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff (Aff, attempt, bracket, cancelWith, catchError, delay, error, fiberCanceler, finally, forkAff, launchAff, launchAff_, message, never, nonCanceler, parallel, runAff, runAff_, sequential, supervise, throwError, try) as Aff
import Effect.Aff.Class (class MonadAff, liftAff) as AffClass
import Effect.Class (class MonadEffect, liftEffect) as EffClass
import Control.Monad.State (class MonadState, class MonadTrans, StateT(..), evalState, evalStateT, execState, execStateT, get, gets, lift, mapState, mapStateT, modify, modify_, put, runState, runStateT, state, withState, withStateT) as MonadState
import Effect.Class.Console hiding (error) as Console
import Prim hiding (Row) as Prim
import Type.Prelude (Proxy(..))
import PSCI.Support (class Eval) as PSCISupport
