module FatPrelude
  ( module Prelude
  , module MonoidUtils
  , module ArrayUtils
  , module CharUtils
  , module IntUtils
  , module Foldable
  , module Map
  , module Set
  , module Maybe
  , module Bifunctor
  , module NonEmptyArray
  , module Tuple.Nested
  , module Type.Prelude
  , module Effect
  , module Unit
  , module Function
  , module Char
  , module CodeUnits
  , module AffClass
  , module EffClass
  ) where

import Prelude

import App.Utils.MonoidUtils (whenMonoid) as MonoidUtils
import App.Utils.ArrayUtils (class Range, getElemSat, getNextElemSat, getPrevElemSat, range, satIndex, (!!!), (..)) as ArrayUtils
import App.Utils.CharUtils (nextChar, prevChar) as CharUtils
import App.Utils.IntUtils (dec, inc) as IntUtils
import Data.Array.NonEmpty hiding ((..), range, all, any, elem, notElem, find, findMap, foldM, intercalate, length) as NonEmptyArray
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Bifunctor
import Data.Char (fromCharCode, toCharCode) as Char
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, surround, surroundMap, traverse_) as Foldable
import Data.Function hiding (apply) as Function
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Maybe
import Data.Set (Set) as Set
import Data.String.CodeUnits (fromCharArray, toCharArray) as CodeUnits
import Data.Tuple.Nested (type (/\), T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry1, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, get1, get10, get2, get3, get4, get5, get6, get7, get8, get9, over1, over10, over2, over3, over4, over5, over6, over7, over8, over9, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\)) as Tuple.Nested
import Data.Unit (Unit, unit) as Unit
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff.Class (class MonadAff, liftAff) as AffClass
import Effect.Class (class MonadEffect, liftEffect) as EffClass
import Prim hiding (Row) as Prim
import Type.Prelude (Proxy(..))
