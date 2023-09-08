module FatPrelude
  ( module Prelude
  , module MonoidUtils
  , module ArrayUtils
  , module CharUtils
  , module StringUtils
  , module NumberUtils
  , module FunctorUtils
  , module MaybeUtils
  , module CommonUtils
  , module Foldable
  , module Traversable
  , module Bitraversable
  , module SemiFoldable
  , module Filterable
  , module Map
  , module Enum
  , module Set
  , module Maybe
  , module Either
  , module Bifunctor
  , module NonEmptyArray
  , module Tuple
  , module Tuple.Nested
  , module Type.Prelude
  , module Effect
  , module Aff
  , module Exception
  , module Unit
  , module Function
  , module Char
  , module String
  , module Int
  , module CodeUnits
  , module AffClass
  , module EffClass
  , module MonadState
  , module Console
  , module PSCISupport
  , module PointFree
  ) where

import Prelude

import App.Utils.Array (class Range, arr2, deleteAt', distance, drop', dropEnd', findIndex', findLastIndex', getElemSat, getNextElemSat, getPrevElemSat, head', inRange, init', insertAt', last', maybeToArray, range, satIndex, slice', sliceNext', splitAt', switchElements, tail', take', takeEnd', toArray', updateAt', zip', (!!!), (..)) as ArrayUtils
import App.Utils.Char (isAplha, isLower, isUpper, nextChar, prevChar) as CharUtils
import App.Utils.Common (partialMaybe) as CommonUtils
import App.Utils.Functor (mapp, (<$$>)) as FunctorUtils
import App.Utils.Maybe (toMaybe, toMaybe', wrapMaybe) as MaybeUtils
import App.Utils.Monoid (whenMonoid, whenMonoidAppend, (<>?)) as MonoidUtils
import App.Utils.Number (abs, coalesce, dec, inc, neg, pos) as NumberUtils
import App.Utils.String (newline, tab, wrap, wrapBackQuotes, wrapBoth, wrapQuotes) as StringUtils
import Control.Monad.State (class MonadState, class MonadTrans, StateT(..), evalState, evalStateT, execState, execStateT, get, gets, lift, mapState, mapStateT, modify, modify_, put, runState, runStateT, state, withState, withStateT) as MonadState
import Data.Array.NonEmpty hiding (all, any, elem, filter, find, findMap, foldM, intercalate, length, notElem, partition, range, scanl, scanr, (..)) as NonEmptyArray
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Bifunctor
import Data.Bitraversable (class Bifoldable, class Bitraversable, biall, biany, bifold, bifoldMap, bifoldMapDefaultL, bifoldMapDefaultR, bifoldl, bifoldlDefault, bifoldr, bifoldrDefault, bifor, bifor_, bisequence, bisequenceDefault, bisequence_, bitraverse, bitraverseDefault, bitraverse_, lfor, ltraverse, rfor, rtraverse) as Bitraversable
import Data.Char (fromCharCode, toCharCode) as Char
import Data.Either (Either(..), blush, choose, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note') as Either
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, defaultCardinality, defaultFromEnum, defaultPred, defaultSucc, defaultToEnum, downFrom, downFromIncluding, enumFromThenTo, enumFromTo, fromEnum, pred, succ, toEnum, toEnumWithDefaults, upFrom, upFromIncluding) as Enum
import Data.Filterable (class Compactable, class Filterable, cleared, compact, eitherBool, filter, filterDefault, filterDefaultPartition, filterDefaultPartitionMap, filterMap, filterMapDefault, maybeBool, partition, partitionDefault, partitionDefaultFilter, partitionDefaultFilterMap, partitionMap, partitionMapDefault, separate) as Filterable
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, or, product, sequence_, sum, surround, surroundMap, traverse_) as Foldable
import Data.Function (applyFlipped, applyN, compose, const, flip, identity, on, (#), ($), (<<<), (>>>)) as Function
import Data.Int (Parity(..), Radix, base36, binary, ceil, decimal, even, floor, fromNumber, fromString, fromStringAs, hexadecimal, octal, odd, parity, pow, quot, radix, rem, round, toNumber, toStringAs, trunc) as Int
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Maybe
import Data.Semigroup.Foldable (intercalateMap) as SemiFoldable
import Data.Set (Set) as Set
import Data.String.CodeUnits (fromCharArray, toCharArray) as CodeUnits
import Data.String.Common hiding (null) as String
import Data.Traversable (class Traversable, Accum, mapAccumL, mapAccumR, scanl, scanr, sequence, sequenceDefault, sequence_, traverse, traverseDefault, traverse_) as Traversable
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as Tuple
import Data.Tuple.Nested (type (/\), Tuple1, Tuple2, Tuple3, Tuple4, Tuple5, curry1, curry2, curry3, curry4, curry5, get1, get2, get3, get4, get5, over1, over2, over3, over4, over5, tuple1, tuple2, tuple3, tuple4, tuple5, uncurry1, uncurry2, uncurry3, uncurry4, uncurry5, (/\)) as Tuple.Nested
import Data.Unit (Unit, unit) as Unit
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff (Aff, attempt, bracket, cancelWith, catchError, delay, error, fiberCanceler, finally, forkAff, launchAff, launchAff_, message, never, nonCanceler, parallel, runAff, runAff_, sequential, supervise, throwError, try) as Aff
import Effect.Aff.Class (class MonadAff, liftAff) as AffClass
import Effect.Class (class MonadEffect, liftEffect) as EffClass
import Effect.Class.Console hiding (error) as Console
import Effect.Exception (error, throw) as Exception
import PSCI.Support (class Eval) as PSCISupport
import PointFree (applySecond, applySecondFlipped, applyThird, applyThirdFlipped, compose2, compose2Flipped, compose2Second, compose2SecondFlipped, compose2Third, compose2ThirdFlipped, compose3, compose3Flipped, compose3Second, compose3SecondFlipped, composeSecond, composeSecondFlipped, composeThird, composeThirdFlipped, (#~), (#~~), (#~~~), (...>), (..>), (.>), (<.), (<..), (<...), (<~.), (<~..), (<~...), (<~~.), (<~~..), (<~~~.), (~$), (~...>), (~..>), (~.>), (~~$), (~~..>), (~~.>), (~~~$), (~~~.>))
import Prim hiding (Row) as Prim
import Type.Prelude (Proxy(..))
