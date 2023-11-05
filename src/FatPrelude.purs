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
  , module TreeUtils
  , module FoldableUtils
  , module UnfoldableUtils
  , module Traversable
  , module Bitraversable
  , module SemiFoldable
  , module Filterable
  , module List
  , module Map
  , module Enum
  , module Set
  , module NonEmptySet
  , module Maybe
  , module Either
  , module MaybeT
  , module ExceptT
  , module Bifunctor
  , module NonEmptyArray
  , module Tuple
  , module Tuple.Nested
  , module Trans
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
  , module MonadError
  , module PSCISupport
  , module PointFree
  , module Foldable
  , module Newtype
  ) where

import Prelude

import App.Utils.Array (arr2, catMaybes', deleteAt', drop', dropEnd', filterByIndexes, findIndex', findLastIndex', getElemSat, getNextElemSat, getPrevElemSat, head', inRange, index', init', insertAt', intersperse', last', maybeToArray, satIndex, slice', sliceNext', splitAt', switchElements, tail', take', takeEnd', toArray', uncons', unsnoc', updateAt', zip', zipWith', (!!!)) as ArrayUtils
import App.Utils.Char (isAplha, isLower, isUpper, nextChar, prevChar) as CharUtils
import App.Utils.Common (partialMaybe) as CommonUtils
import App.Utils.Foldable (findMapEither, intercalate1, maximum1, maximumBy1, minimum1, minimumBy1) as FoldableUtils
import App.Utils.Functor (mapp, (<$$>)) as FunctorUtils
import App.Utils.Maybe (toMaybe, toMaybe', wrapMaybe) as MaybeUtils
import App.Utils.Monoid (unlessMonoid, whenMonoid, (<>?)) as MonoidUtils
import App.Utils.Number (abs, coalesce, dec, inc, neg, pos, zeroOrNeg, zeroOrPos) as NumberUtils
import App.Utils.String (newline, showParensCsv, str, tab, wrapBackQuotes, wrapBoth, wrapCurly, wrapParens, wrapQuotes) as StringUtils
import App.Utils.Tree (ancestorsValues, appendChildren, childrenValues, findValues, goToNode, mkLeaf, nodeValues, siblingsValues) as TreeUtils
import App.Utils.Unfoldable (class Range, range, (..)) as UnfoldableUtils
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, catchJust, liftEither, liftMaybe, throwError, try, withResource) as MonadError
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, runExceptT, withExceptT) as ExceptT
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT, runMaybeT) as MaybeT
import Control.Monad.State (class MonadState, StateT(..), evalState, evalStateT, execState, execStateT, get, gets, mapState, mapStateT, modify, modify_, put, runState, runStateT, state, withState, withStateT) as MonadState
import Control.Monad.Trans.Class (class MonadTrans, lift) as Trans
import Data.Array.NonEmpty hiding (all, any, elem, filter, find, findMap, fold1, foldM, foldMap1, foldl1, foldr1, fromFoldable, fromFoldable1, fromNonEmpty, intercalate, length, notElem, partition, range, scanl, scanr, toNonEmpty, toUnfoldable, toUnfoldable1, (..), (:)) as NonEmptyArray
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Bifunctor
import Data.Bitraversable (class Bifoldable, class Bitraversable, biall, biany, bifold, bifoldMap, bifoldl, bifoldr, bifor, bifor_, bisequence, bisequence_, bitraverse, bitraverse_, lfor, ltraverse, rfor, rtraverse) as Bitraversable
import Data.Char (fromCharCode, toCharCode) as Char
import Data.Either (Either(..), blush, choose, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note') as Either
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, downFrom, downFromIncluding, enumFromThenTo, enumFromTo, fromEnum, pred, succ, toEnum, toEnumWithDefaults, upFrom, upFromIncluding) as Enum
import Data.Filterable (class Compactable, class Filterable, cleared, compact, eitherBool, filter, filterMap, maybeBool, partition, partitionMap, separate) as Filterable
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldl, foldr, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, or, product, sequence_, sum, surround, surroundMap, traverse_) as Foldable
import Data.Function (applyN, const, flip, identity, on, (#), ($), (<<<), (>>>)) as Function
import Data.Int (Parity(..), Radix, base36, binary, ceil, decimal, even, floor, fromNumber, fromString, fromStringAs, hexadecimal, octal, odd, parity, pow, quot, radix, rem, round, toNumber, toStringAs, trunc) as Int
import Data.List (List(..), (:)) as List
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Maybe
import Data.Newtype hiding (modify, over2, traverse) as Newtype
import Data.Semigroup.Foldable hiding (intercalate, maximum, maximumBy, minimum, minimumBy) as SemiFoldable
import Data.Set (Set) as Set
import Data.Set.NonEmpty (NonEmptySet) as NonEmptySet
import Data.String.CodeUnits (fromCharArray, toCharArray) as CodeUnits
import Data.String.Common (joinWith, localeCompare, replace, replaceAll, split, toLower, toUpper, trim) as String
import Data.Traversable (class Traversable, Accum, mapAccumL, mapAccumR, scanl, scanr, sequence, sequence_, traverse, traverse_) as Traversable
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as Tuple
import Data.Tuple.Nested (type (/\), Tuple3, Tuple4, curry3, curry4, get1, get2, get3, get4, over1, over2, over3, over4, tuple3, tuple4, uncurry3, uncurry4, (/\)) as Tuple.Nested
import Data.Unit (Unit, unit) as Unit
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff (Aff, attempt, bracket, cancelWith, catchError, delay, error, fiberCanceler, finally, forkAff, launchAff, launchAff_, message, never, nonCanceler, parallel, runAff, runAff_, sequential, supervise, throwError, try) as Aff
import Effect.Aff.Class (class MonadAff, liftAff) as AffClass
import Effect.Class (class MonadEffect, liftEffect) as EffClass
import Effect.Exception (Error, error, throw) as Exception
import PSCI.Support (class Eval) as PSCISupport
import PointFree ((#~), (#~~), (#~~~), (...>), (..>), (.>), (<.), (<..), (<...), (<~.), (<~..), (<~...), (<~~.), (<~~..), (<~~~.), (~$), (~...>), (~..>), (~.>), (~~$), (~~..>), (~~.>), (~~~$), (~~~.>))
import Prim hiding (Row) as Prim
import Type.Prelude (Proxy(..))
